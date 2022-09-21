-module(wapi_payres_handler).

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").

-behaviour(swag_server_payres_logic_handler).
%% swag_server_payres_logic_handler callbacks
-export([map_error/2]).
-export([authorize_api_key/4]).
-export([handle_request/4]).

-behaviour(wapi_handler).
%% wapi_handler callbacks
-export([prepare/4]).

%% Types

-type req_data() :: wapi_handler:req_data().
-type request_state() :: wapi_handler:request_state().
-type handler_context() :: wapi_handler:context().
-type request_result() :: wapi_handler:request_result().
-type operation_id() :: swag_server_payres:operation_id().
-type api_key() :: swag_server_payres:api_key().
-type request_context() :: swag_server_payres:request_context().
-type handler_opts() :: swag_server_payres:handler_opts(_).

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
map_error_type(wrong_array) -> <<"WrongArray">>;
map_error_type(wrong_format) -> <<"WrongFormat">>.

-spec authorize_api_key(operation_id(), api_key(), request_context(), handler_opts()) ->
    Result :: false | {true, wapi_auth:preauth_context()}.
authorize_api_key(OperationID, ApiKey, _Context, _Opts) ->
    %% Since we require the request id field to create a woody context for our trip to token_keeper
    %% it seems it is no longer possible to perform any authorization in this method.
    %% To gain this ability back be would need to rewrite the swagger generator to perform its
    %% request validation checks before this stage.
    %% But since a decent chunk of authorization logic is already defined in the handler function
    %% it is probably easier to move it there in its entirety.
    ok = scoper:add_scope('swag.server', #{api => wallet, operation_id => OperationID}),
    case wapi_auth:preauthorize_api_key(ApiKey) of
        {ok, Context} ->
            {true, Context};
        {error, Error} ->
            _ = logger:info("API Key preauthorization failed for ~p due to ~p", [OperationID, Error]),
            false
    end.

-spec handle_request(operation_id(), req_data(), request_context(), handler_opts()) -> request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    wapi_handler:handle_request(payres, OperationID, Req, SwagContext, Opts).

-spec prepare(operation_id(), req_data(), handler_context(), handler_opts()) -> {ok, request_state()} | no_return().
prepare(OperationID = 'StoreBankCard', #{'BankCard' := CardData}, Context, _Opts) ->
    Authorize = mk_auth_function(OperationID, Context),
    Process = fun() ->
        CVV = maps:get(<<"cvv">>, CardData, undefined),
        {BankCard, AuthData} = process_card_data(CardData, CVV, Context),
        wapi_handler_utils:reply_ok(201, decode_bank_card(BankCard, AuthData))
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID = 'GetBankCard', #{'token' := Token}, Context, _Opts) ->
    Authorize = mk_auth_function(OperationID, Context),
    Process = fun() ->
        try
            wapi_handler_utils:reply_ok(200, decrypt_token(Token))
        catch
            error:badarg ->
                wapi_handler_utils:reply_ok(404)
        end
    end,
    {ok, #{authorize => Authorize, process => Process}}.

%% Internal functions

mk_auth_function(OperationID, Context) ->
    fun() ->
        Prototypes = [{operation, #{id => OperationID}}],
        {ok, wapi_auth:authorize_operation(Prototypes, Context)}
    end.

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
            genlib_map:compact(
                #{
                    <<"token">> => Token,
                    <<"bin">> => Bin,
                    <<"lastDigits">> => LastDigits
                }
            );
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
    ok = validate_card_data(CardDataThrift, ExtraCardData, SessionThrift, PaymentSystem, WoodyContext),
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

validate_card_data(CardData, ExtraCardData, SessionData, PaymentSystem, WoodyContext) ->
    ValidationEnv = validation_env(),
    BankCardData = wapi_bankcard:merge_data(CardData, ExtraCardData, SessionData),
    case bankcard_validator:validate(BankCardData, PaymentSystem, ValidationEnv, WoodyContext) of
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

validation_env() ->
    ValidationEnv = maps:get(env, genlib_app:env(wapi, validation, #{}), #{}),
    DefaultEnv = #{now => calendar:universal_time()},
    maps:merge(DefaultEnv, ValidationEnv).

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
    CardNumber = genlib:to_binary(maps:get(<<"cardNumber">>, CardData)),
    PaymentSystem = wapi_bankcard:payment_system(BankInfo),
    genlib_map:compact(#{
        token => BankCard#cds_BankCard.token,
        pan_length => byte_size(CardNumber),
        bin => get_first6(CardNumber),
        last_digits => get_last4(CardNumber),
        payment_system => PaymentSystem,
        exp_date => ExpDate,
        cardholder_name => genlib_map:get(<<"cardHolder">>, CardData)
    }).

%% NOTE
%% Seems to fit within PCIDSS requirments for all PAN lengths
get_first6(CardNumber) ->
    binary:part(CardNumber, {0, 6}).

get_last4(CardNumber) ->
    binary:part(CardNumber, {byte_size(CardNumber), -4}).

to_thrift(card_data, Data) ->
    CardNumber = genlib:to_binary(maps:get(<<"cardNumber">>, Data)),
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
        payment_system = to_thrift(payment_system, maps:get(payment_system, BankCard)),
        payment_system_deprecated = maps:get(payment_system_deprecated, BankCard),
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
to_thrift(session_data, CVC) when is_binary(CVC) ->
    #cds_SessionData{auth_data = {card_security_code, #cds_CardSecurityCode{value = CVC}}};
to_thrift(session_data, undefined) ->
    #'cds_SessionData'{
        auth_data = {card_security_code, #'cds_CardSecurityCode'{value = <<>>}}
    };
to_thrift(payment_system, undefined) ->
    undefined;
to_thrift(payment_system, ID) ->
    #'PaymentSystemRef'{id = ID}.

decode_bank_card(BankCard, AuthData) ->
    #{
        bin := Bin,
        last_digits := LastDigits
    } = BankCard,
    BankCardThrift = to_thrift(bank_card, BankCard),
    TokenValidUntil = wapi_utils:deadline_from_timeout(resource_token_lifetime()),
    EncryptedToken = wapi_crypto:create_resource_token({bank_card, BankCardThrift}, TokenValidUntil),
    genlib_map:compact(#{
        <<"token">> => EncryptedToken,
        <<"bin">> => Bin,
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
