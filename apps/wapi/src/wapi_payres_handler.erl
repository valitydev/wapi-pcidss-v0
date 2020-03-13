-module(wapi_payres_handler).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").

-behaviour(swag_server_payres_logic_handler).
-behaviour(wapi_handler).

%% swag_server_payres_logic_handler callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% wapi_handler callbacks
-export([process_request/4]).

%% Types

-type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type request_result()  :: wapi_handler:request_result().
-type operation_id()    :: swag_server_payres:operation_id().
-type api_key()         :: swag_server_payres:api_key().
-type request_context() :: swag_server_payres:request_context().
-type handler_opts()    :: swag_server_payres:handler_opts(term()).

%% API

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey, Opts) ->
    scope(OperationID, fun () ->
        wapi_auth:authorize_api_key(OperationID, ApiKey, Opts)
    end).

-spec handle_request(operation_id(), req_data(), request_context(), handler_opts()) ->
    request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    scope(OperationID, fun () ->
        wapi_handler:handle_request(OperationID, Req, SwagContext, ?MODULE, Opts)
    end).

scope(OperationID, Fun) ->
    scoper:scope(swagger, #{api => payres, operation_id => OperationID}, Fun).

-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) ->
    request_result().
process_request('StoreBankCard', #{'BankCard' := CardData}, Context, _Opts) ->
    CVV = maps:get(<<"cvv">>, CardData, undefined),
    {BankCard, AuthData} = process_card_data(CardData, CVV, Context),
    wapi_handler_utils:reply_ok(201, decode_bank_card(BankCard, AuthData));
process_request('GetBankCard', #{'token' := Token}, _Context, _Opts) ->
    try wapi_handler_utils:reply_ok(200, decrypt_token(Token))
    catch
        error:badarg ->
            wapi_handler_utils:reply_ok(404)
    end.

%% Internal functions

decrypt_token(Token) ->
    case wapi_crypto:decrypt_bankcard_token(Token) of
        unrecognized ->
            decode_legacy_token(Token);
        {ok, BankCard} ->
            LastDigits = wapi_utils:get_last_pan_digits(BankCard#'BankCard'.masked_pan),
            Bin = BankCard#'BankCard'.bin,
            #{
                <<"token">> => Token,
                <<"bin">> => Bin,
                <<"lastDigits">> => LastDigits,
                <<"paymentSystem">> => genlib:to_binary(BankCard#'BankCard'.payment_system)
            };
        {error, {decryption_failed, _} = Error} ->
            logger:warning("Bank card token decryption failed: ~p", [Error]),
            erlang:error(badarg)
    end.

decode_legacy_token(Token) ->
    case wapi_utils:base64url_to_map(Token) of
        Data = #{<<"token">> := _} ->
            CardData = maps:with([<<"bin">>, <<"lastDigits">>], Data),
            CardData#{<<"token">> => Token};
        _ ->
            erlang:error(badarg)
    end.

process_card_data(CardData, CVV, Context) ->
    CardDataThrift = to_thrift(card_data, CardData),
    SessionThrift  = to_thrift(session_data, CVV),
    {BankCardCDS, BankInfo} = put_card_to_cds(CardDataThrift, Context),
    SessionID   = put_session_to_cds(SessionThrift, Context),
    BankCard = construct_bank_card(BankCardCDS, CardData, BankInfo),
    case CVV of
        V when is_binary(V) ->
            {BankCard, SessionID};
        undefined ->
            {BankCard, undefined}
    end.

put_card_to_cds(CardData, #{woody_context := WoodyContext}) ->
    case wapi_bankcard:lookup_bank_info(CardData#cds_PutCardData.pan, WoodyContext) of
        {ok, BankInfo} ->
            Call = {cds_storage, 'PutCard', [CardData]},
            case service_call(Call, WoodyContext) of
                {ok, #cds_PutCardResult{bank_card = BankCard}} ->
                    {BankCard, BankInfo};
                {exception, #cds_InvalidCardData{}} ->
                    wapi_handler:throw_result(wapi_handler_utils:reply_ok(422,
                        wapi_handler_utils:get_error_msg(<<"Card data is invalid">>)
                    ))
            end;
        {error, _Reason} ->
            wapi_handler:throw_result(wapi_handler_utils:reply_ok(422,
                wapi_handler_utils:get_error_msg(<<"Unsupported card">>)
            ))
    end.

put_session_to_cds(SessionData, #{woody_context := WoodyContext}) ->
    SessionID = make_random_id(),
    Call = {cds_storage, 'PutSession', [SessionID, SessionData]},
    {ok, ok} = service_call(Call, WoodyContext),
    SessionID.

make_random_id() ->
    Random = crypto:strong_rand_bytes(16),
    genlib_format:format_int_base(binary:decode_unsigned(Random), 62).

construct_bank_card(BankCard, CardData, BankInfo) ->
    ExpDate = parse_exp_date(genlib_map:get(<<"expDate">>, CardData)),
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, CardData)),
    Bin = BankCard#cds_BankCard.bin,
    LastDigits = BankCard#cds_BankCard.last_digits,
    genlib_map:compact(#{
        token => BankCard#cds_BankCard.token,
        pan_length => byte_size(CardNumber),
        bin => Bin,
        last_digits => LastDigits,
        payment_system => maps:get(payment_system, BankInfo),
        exp_date => ExpDate,
        cardholder_name => genlib_map:get(<<"cardHolder">>, CardData)
    }).

to_thrift(card_data, Data) ->
    ExpDate = case parse_exp_date(genlib_map:get(<<"expDate">>, Data)) of
        {Month, Year} ->
            #cds_ExpDate{
                month = Month,
                year = Year
            };
        undefined ->
            undefined
    end,
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, Data)),
    #cds_PutCardData{
        pan  = CardNumber,
        exp_date = ExpDate,
        cardholder_name = genlib_map:get(<<"cardHolder">>, Data)
    };
to_thrift(bank_card, BankCard) ->
    ExpDate = genlib_map:get(exp_date, BankCard),
    #'BankCard'{
        token = maps:get(token, BankCard),
        bin = maps:get(bin, BankCard),
        masked_pan = maps:get(last_digits, BankCard),
        payment_system = maps:get(payment_system, BankCard),
        exp_date = to_thrift(exp_date, ExpDate),
        cardholder_name = genlib_map:get(cardholder_name, BankCard)
    };

to_thrift(exp_date, undefined) ->
    undefined;
to_thrift(exp_date, {Month, Year}) ->
    #'BankCardExpDate'{
        month = Month,
        year = Year
    };
to_thrift(session_data, CVV) when is_binary(CVV) ->
    #cds_SessionData{ auth_data = {card_security_code, #cds_CardSecurityCode{ value = CVV }}};
to_thrift(session_data, undefined) ->
    #'cds_SessionData'{
        auth_data = {card_security_code, #'cds_CardSecurityCode'{value = <<>>}}
    }.

decode_bank_card(BankCard, AuthData) ->
    #{
        bin := Bin,
        last_digits := LastDigits,
        payment_system := PaymentSystem
    } = BankCard,
    EncryptedToken = wapi_crypto:encrypt_bankcard_token(to_thrift(bank_card, BankCard)),
    genlib_map:compact(#{
        <<"token">>          => EncryptedToken,
        <<"bin">>            => Bin,
        <<"paymentSystem">>  => genlib:to_binary(PaymentSystem),
        <<"lastDigits">>     => LastDigits,
        <<"authData">>       => decode_auth_data(AuthData)
    }).

decode_auth_data(PaymentSessionID) when is_binary(PaymentSessionID) ->
    genlib:to_binary(PaymentSessionID);
decode_auth_data(undefined) ->
    undefined.

parse_exp_date(undefined) ->
    undefined;
parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month, Year0] = binary:split(ExpDate, <<"/">>),
    Year = case genlib:to_int(Year0) of
        Y when Y < 100 ->
            2000 + Y;
        Y ->
            Y
    end,
    {genlib:to_int(Month), Year}.

service_call({ServiceName, Function, Args}, WoodyContext) ->
    wapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).
