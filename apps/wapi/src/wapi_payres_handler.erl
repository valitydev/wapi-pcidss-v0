-module(wapi_payres_handler).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

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
-type handler_opts()    :: swag_server_payres:handler_opts().

%% API

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey, Opts) ->
    ok = scoper:add_meta(#{api => payres, operation_id => OperationID}),
    wapi_auth:authorize_api_key(OperationID, ApiKey, Opts).

-spec handle_request(operation_id(), req_data(), request_context(), handler_opts()) ->
    request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    wapi_handler:handle_request(OperationID, Req, SwagContext, ?MODULE, Opts).

-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) ->
    request_result().
process_request('StoreBankCard', #{'BankCard' := CardData}, Context, _Opts) ->
    CVV = maps:get(<<"cvv">>, CardData, undefined),
    {BankCard, AuthData} = process_card_data(CardData, CVV, Context),
    wapi_handler_utils:reply_ok(201, maps:merge(
        to_swag(bank_card, construct_token(BankCard)),
        to_swag(auth_data, AuthData)
    ));
process_request('GetBankCard', #{'token' := Token}, _Context, _Opts) ->
    try wapi_handler_utils:reply_ok(200, to_swag(bank_card, Token))
    catch
        error:badarg ->
            wapi_handler_utils:reply_ok(404)
    end.

%% Internal functions

process_card_data(CardData, CVV, Context) ->
    {BankCard, SessionID} = put_card_data_to_cds(to_thrift(card_data, CardData), to_thrift(session_data, CVV), Context),
    case CVV of
        V when is_binary(V) ->
            {BankCard, SessionID};
        undefined ->
            {BankCard, undefined}
    end.

put_card_data_to_cds(CardData, SessionData, Context) ->
    Call = {cds_storage, 'PutCardData', [CardData, SessionData]},
    case service_call(Call, Context) of
        {ok, #'PutCardDataResult'{session_id = SessionID, bank_card = BankCard}} ->
            {BankCard, SessionID};
        {exception, #'InvalidCardData'{}} ->
            wapi_handler:throw_result(wapi_handler_utils:reply_ok(422,
                wapi_handler_utils:get_error_msg(<<"Card data is invalid">>)
            ))
    end.

to_thrift(card_data, Data) ->
    {Month, Year} = parse_exp_date(genlib_map:get(<<"expDate">>, Data)),
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, Data)),
    #'CardData'{
        pan  = CardNumber,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = genlib_map:get(<<"cardHolder">>, Data, undefined),
        cvv             = genlib_map:get(<<"cvv">>, Data, undefined)
    };
to_thrift(session_data, CVV) when is_binary(CVV) ->
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{value = CVV}}
    };
to_thrift(session_data, undefined) ->
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{value = <<>>}}
    }.

construct_token(BankCard = #domain_BankCard{}) ->
    wapi_utils:map_to_base64url(genlib_map:compact(#{
        <<"token">>          => BankCard#domain_BankCard.token,
        <<"paymentSystem">>  => genlib:to_binary(BankCard#domain_BankCard.payment_system),
        <<"bin">>            => BankCard#domain_BankCard.bin,
        <<"lastDigits">>     => wapi_utils:get_last_pan_digits(BankCard#domain_BankCard.masked_pan)
    })).

to_swag(bank_card, Token) when is_binary(Token) ->
    case wapi_utils:base64url_to_map(Token) of
        Data = #{<<"token">> := _} ->
            Presentation = maps:with([<<"token">>, <<"paymentSystem">>, <<"bin">>, <<"lastDigits">>], Data),
            Presentation#{<<"token">> => Token};
        _ ->
            erlang:error(badarg)
    end;
to_swag(auth_data, PaymentSessionID) when is_binary(PaymentSessionID) ->
    #{<<"authData">> => genlib:to_binary(PaymentSessionID)};
to_swag(auth_data, undefined) ->
    #{}.

parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month, Year0] = binary:split(ExpDate, <<"/">>),
    Year = case genlib:to_int(Year0) of
        Y when Y < 100 ->
            2000 + Y;
        Y ->
            Y
    end,
    {genlib:to_int(Month), Year}.

service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    wapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).
