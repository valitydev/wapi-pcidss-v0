-module(wapi_payres_handler).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").

-behaviour(swag_server_payres_logic_handler).
-behaviour(wapi_handler).

%% swag_server_payres_logic_handler callbacks
-export([map_error/2]).
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% wapi_handler callbacks
-export([process_request/4]).

%% Types

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type request_result() :: wapi_handler:request_result().
-type operation_id() :: swag_server_payres:operation_id().
-type api_key() :: swag_server_payres:api_key().
-type request_context() :: swag_server_payres:request_context().
-type handler_opts() :: swag_server_payres:handler_opts(term()).

%% Macro

-define(DEFAULT_RESOURCE_TOKEN_LIFETIME, <<"64m">>).

%% API

-spec map_error(atom(), swag_server_payres_validation:error()) -> swag_server_payres:error_reason().
map_error(validation_error, Error) ->
    Type = map_error_type(maps:get(type, Error)),
    Name = genlib:to_binary(maps:get(param_name, Error)),
    Message =
        case maps:get(description, Error, undefined) of
            undefined ->
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary>>;
            Description ->
                DescriptionBin = genlib:to_binary(Description),
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary, ", description: ",
                    DescriptionBin/binary>>
        end,
    jsx:encode(#{
        <<"errorType">> => Type,
        <<"name">> => Name,
        <<"description">> => Message
    }).

-spec map_error_type(swag_server_payres_validation:error_type()) -> binary().
map_error_type(no_match) -> <<"NoMatch">>;
map_error_type(not_found) -> <<"NotFound">>;
map_error_type(not_in_range) -> <<"NotInRange">>;
map_error_type(wrong_length) -> <<"WrongLength">>;
map_error_type(wrong_size) -> <<"WrongSize">>;
map_error_type(schema_violated) -> <<"SchemaViolated">>;
map_error_type(wrong_type) -> <<"WrongType">>;
map_error_type(wrong_array) -> <<"WrongArray">>.

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) -> false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey, Opts) ->
    scope(OperationID, fun() ->
        wapi_auth:authorize_api_key(OperationID, ApiKey, Opts)
    end).

-spec handle_request(operation_id(), req_data(), request_context(), handler_opts()) -> request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    scope(OperationID, fun() ->
        wapi_handler:handle_request(OperationID, Req, SwagContext, ?MODULE, Opts)
    end).

scope(OperationID, Fun) ->
    scoper:scope(swagger, #{api => payres, operation_id => OperationID}, Fun).

-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) -> request_result().
process_request('StoreBankCard', #{'BankCard' := CardData}, Context, _Opts) ->
    CVV = maps:get(<<"cvv">>, CardData, undefined),
    {BankCard, AuthData} = process_card_data(CardData, CVV, Context),
    wapi_handler_utils:reply_ok(201, decode_bank_card(BankCard, AuthData));
process_request('GetBankCard', #{'token' := Token}, _Context, _Opts) ->
    try
        wapi_handler_utils:reply_ok(200, decrypt_token(Token))
    catch
        error:badarg ->
            wapi_handler_utils:reply_ok(404)
    end.

%% Internal functions

-spec resource_token_lifetime() -> timeout().
resource_token_lifetime() ->
    case genlib_app:env(wapi, resource_token_lifetime, ?DEFAULT_RESOURCE_TOKEN_LIFETIME) of
        Value when is_integer(Value) ->
            Value;
        Value ->
            case wapi_utils:parse_lifetime(Value) of
                {ok, Lifetime} ->
                    Lifetime;
                Error ->
                    erlang:error(Error, [Value])
            end
    end.

decrypt_token(Token) ->
    case wapi_crypto:decrypt_resource_token(Token) of
        unrecognized ->
            decode_legacy_token(Token);
        {ok, {Resource, ValidUntil}} ->
            {bank_card, BankCard} =
                case wapi_utils:deadline_is_reached(ValidUntil) of
                    true ->
                        logger:warning("Resource token expired: ~p", [wapi_utils:deadline_to_binary(ValidUntil)]),
                        erlang:error(badarg);
                    _ ->
                        Resource
                end,
            LastDigits = wapi_utils:get_last_pan_digits(BankCard#'BankCard'.masked_pan),
            Bin = BankCard#'BankCard'.bin,
            #{
                <<"token">> => Token,
                <<"bin">> => Bin,
                <<"lastDigits">> => LastDigits,
                <<"paymentSystem">> => genlib:to_binary(BankCard#'BankCard'.payment_system)
            };
        {error, {decryption_failed, _} = Error} ->
            logger:warning("Resource token decryption failed: ~p", [Error]),
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

process_card_data(CardData, CVV, #{woody_context := WoodyContext} = Context) ->
    {CardDataThrift, ExtraCardData} = to_thrift(card_data, CardData),
    SessionThrift = to_thrift(session_data, CVV),
    {ok, BankInfo} = lookup_bank_info(CardDataThrift#cds_PutCardData.pan, WoodyContext),
    PaymentSystem = wapi_bankcard:payment_system(BankInfo),
    ok = validate_card_data(CardDataThrift, ExtraCardData, SessionThrift, PaymentSystem),
    {BankCardCDS, BankInfo} = put_card_to_cds(CardDataThrift, BankInfo, Context),
    SessionID = put_session_to_cds(SessionThrift, Context),
    BankCard = construct_bank_card(BankCardCDS, CardData, BankInfo),
    case CVV of
        V when is_binary(V) ->
            {BankCard, SessionID};
        undefined ->
            {BankCard, undefined}
    end.

put_card_to_cds(CardData, BankInfo, #{woody_context := WoodyContext}) ->
    Call = {cds_storage, 'PutCard', {CardData}},
    case service_call(Call, WoodyContext) of
        {ok, #cds_PutCardResult{bank_card = BankCard}} ->
            {BankCard, BankInfo};
        {exception, #cds_InvalidCardData{}} ->
            wapi_handler:throw_result(
                wapi_handler_utils:reply_ok(
                    422,
                    wapi_handler_utils:get_error_msg(<<"Card data is invalid">>)
                )
            )
    end.

lookup_bank_info(Pan, WoodyContext) ->
    case wapi_bankcard:lookup_bank_info(Pan, WoodyContext) of
        {ok, BankInfo} ->
            {ok, BankInfo};
        {error, _Reason} ->
            wapi_handler:throw_result(
                wapi_handler_utils:reply_ok(
                    422,
                    wapi_handler_utils:get_error_msg(<<"Unsupported card">>)
                )
            )
    end.

validate_card_data(CardData, ExtraCardData, SessionData, PaymentSystem) ->
    ValidationEnv = maps:get(env, genlib_app:env(wapi, validation, #{}), #{}),
    case wapi_bankcard:validate(CardData, ExtraCardData, SessionData, PaymentSystem, ValidationEnv) of
        ok ->
            ok;
        {error, _Error} ->
            wapi_handler:throw_result(
                wapi_handler_utils:reply_ok(
                    422,
                    wapi_handler_utils:get_error_msg(<<"Card data is invalid">>)
                )
            )
    end.

put_session_to_cds(SessionData, #{woody_context := WoodyContext}) ->
    SessionID = make_random_id(),
    Call = {cds_storage, 'PutSession', {SessionID, SessionData}},
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
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, Data)),
    ExpDate = parse_exp_date(genlib_map:get(<<"expDate">>, Data)),
    Cardholder = genlib_map:get(<<"cardHolder">>, Data),
    {
        #cds_PutCardData{
            pan = CardNumber
        },
        genlib_map:compact(#{
            cardholder => Cardholder,
            exp_date => ExpDate
        })
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
    #cds_SessionData{auth_data = {card_security_code, #cds_CardSecurityCode{value = CVV}}};
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
    BankCardThrift = to_thrift(bank_card, BankCard),
    TokenValidUntil = wapi_utils:deadline_from_timeout(resource_token_lifetime()),
    EncryptedToken = wapi_crypto:create_resource_token({bank_card, BankCardThrift}, TokenValidUntil),
    genlib_map:compact(#{
        <<"token">> => EncryptedToken,
        <<"bin">> => Bin,
        <<"paymentSystem">> => genlib:to_binary(PaymentSystem),
        <<"lastDigits">> => LastDigits,
        <<"authData">> => decode_auth_data(AuthData),
        <<"validUntil">> => decode_deadline(TokenValidUntil)
    }).

decode_deadline(undefined) ->
    undefined;
decode_deadline(Deadline) ->
    wapi_utils:deadline_to_binary(Deadline).

decode_auth_data(PaymentSessionID) when is_binary(PaymentSessionID) ->
    genlib:to_binary(PaymentSessionID);
decode_auth_data(undefined) ->
    undefined.

parse_exp_date(undefined) ->
    undefined;
parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month, Year0] = binary:split(ExpDate, <<"/">>),
    Year =
        case genlib:to_int(Year0) of
            Y when Y < 100 ->
                2000 + Y;
            Y ->
                Y
        end,
    {genlib:to_int(Month), Year}.

service_call({ServiceName, Function, Args}, WoodyContext) ->
    wapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).
