-module(wapi_handler).

%% API
-export([handle_request/5]).
-export([throw_result/1]).

%% Behaviour definition

-type tag() :: payres.

-type operation_id() :: swag_server_payres:operation_id().
-type swagger_context() :: swag_server_payres:request_context().

-type context() :: #{
    operation_id := operation_id(),
    woody_context := woody_context:ctx(),
    swagger_context := swagger_context()
}.

-type opts() ::
    swag_server_payres:handler_opts(_).

-type req_data() :: #{atom() | binary() => term()}.
-type status_code() :: 200..599.
-type headers() :: cowboy:http_headers().
-type response_data() :: map() | [map()] | undefined.
-type response() :: {status_code(), headers(), response_data()}.
-type request_result() :: {ok | error, response()}.

-callback prepare(
    OperationID :: operation_id(),
    Req :: req_data(),
    Context :: context(),
    Opts :: opts()
) -> {ok, request_state()} | no_return().

-type throw(_T) :: no_return().

-type request_state() :: #{
    authorize := fun(() -> {ok, wapi_auth:resolution()} | throw(response())),
    process := fun(() -> {ok, response()} | throw(request_result()))
}.

-export_type([request_state/0]).
-export_type([response/0]).
-export_type([context/0]).
-export_type([req_data/0]).
-export_type([status_code/0]).
-export_type([response_data/0]).
-export_type([headers/0]).
-export_type([request_result/0]).

%% API
%% @WARNING Must be refactored in case of different classes of users using this API
%% See CAPI capi_handler
%% https://github.com/valitydev/capi-v2/blob/2de9367561a511f0dc1448881201de48e9004c54/apps/capi/src/capi_handler.erl#L62
-define(REALM, <<"external">>).

-define(REQUEST_RESULT, wapi_req_result).

-spec handle_request(tag(), operation_id(), req_data(), swagger_context(), opts()) -> request_result().
handle_request(Tag, OperationID, Req, SwagContext, Opts) ->
    ok = set_otel_context(SwagContext),
    try
        WoodyContext = attach_deadline(Req, create_woody_context(Tag, Req)),
        process_request(Tag, OperationID, Req, SwagContext, Opts, WoodyContext)
    catch
        throw:({bad_deadline, _Header}) ->
            wapi_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"SchemaViolated">>,
                <<"name">> => <<"X-Request-Deadline">>,
                <<"description">> => <<"Invalid data in X-Request-Deadline header">>
            })
    end.

process_request(Tag, OperationID, Req, SwagContext0, Opts, WoodyContext0) ->
    _ = logger:info("Processing request ~p", [OperationID]),
    try
        SwagContext = do_authorize_api_key(SwagContext0, WoodyContext0),
        AuthContext = get_auth_context(SwagContext),
        WoodyContext = put_user_identity(WoodyContext0, AuthContext),
        Context = create_handler_context(OperationID, SwagContext, WoodyContext),
        ok = set_context_meta(AuthContext),
        Handler = get_handler(Tag),
        {ok, RequestState} = Handler:prepare(OperationID, Req, Context, Opts),
        #{authorize := Authorize, process := Process} = RequestState,
        {ok, Resolution} = Authorize(),
        case Resolution of
            allowed ->
                ok = logger:debug("Operation ~p authorized", [OperationID]),
                Process();
            forbidden ->
                wapi_handler_utils:reply_ok(401)
        end
    catch
        throw:{token_auth_failed, Reason} ->
            _ = logger:info("API Key authorization failed for ~p due to ~p", [OperationID, Reason]),
            wapi_handler_utils:reply_ok(401);
        throw:{?REQUEST_RESULT, Result} ->
            Result;
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details)
    end.

-spec throw_result(request_result()) -> no_return().
throw_result(Res) ->
    erlang:throw({?REQUEST_RESULT, Res}).

get_handler(payres) -> wapi_payres_handler.

-spec create_woody_context(tag(), req_data()) -> woody_context:ctx().
create_woody_context(Tag, #{'X-Request-ID' := RequestID}) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    ok = scoper:add_meta(#{request_id => RequestID, trace_id => TraceID}),
    woody_context:new(RpcID, undefined, wapi_woody_client:get_service_deadline(Tag)).

-spec create_handler_context(operation_id(), swagger_context(), woody_context:ctx()) -> context().
create_handler_context(OpID, SwagContext, WoodyContext) ->
    #{
        operation_id => OpID,
        woody_context => WoodyContext,
        swagger_context => SwagContext
    }.

process_woody_error(_Source, result_unexpected, _Details) ->
    wapi_handler_utils:reply_error(500);
process_woody_error(_Source, resource_unavailable, _Details) ->
    % Return an 504 since it is unknown if state of the system has been altered
    % @TODO Implement some sort of tagging for operations that mutate the state,
    % so we can still return 503s for those that don't
    wapi_handler_utils:reply_error(504);
process_woody_error(_Source, result_unknown, _Details) ->
    wapi_handler_utils:reply_error(504).

do_authorize_api_key(#{auth_context := PreAuthContext} = SwagContext, WoodyContext) ->
    case wapi_auth:authorize_api_key(PreAuthContext, make_token_context(SwagContext), WoodyContext) of
        {ok, AuthContext} ->
            SwagContext#{auth_context => AuthContext};
        {error, Error} ->
            throw({token_auth_failed, Error})
    end.

make_token_context(#{cowboy_req := CowboyReq}) ->
    case cowboy_req:header(<<"origin">>, CowboyReq) of
        Origin when is_binary(Origin) ->
            #{request_origin => Origin};
        undefined ->
            #{}
    end.

put_user_identity(WoodyContext, AuthContext) ->
    woody_user_identity:put(collect_user_identity(AuthContext), WoodyContext).

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id => wapi_auth:get_subject_id(AuthContext),
        %%TODO: Store user realm in authdata meta and extract it here
        realm => ?REALM,
        email => wapi_auth:get_user_email(AuthContext)
    }).

attach_deadline(#{'X-Request-Deadline' := Header}, Context) ->
    case wapi_utils:parse_deadline(Header) of
        {ok, Deadline} when Deadline /= undefined ->
            woody_context:set_deadline(Deadline, Context);
        _ ->
            throw({bad_deadline, Header})
    end;
attach_deadline(_, Context) ->
    Context.

set_context_meta(AuthContext) ->
    Meta = #{
        metadata => #{
            'user-identity' => collect_user_identity(AuthContext)
        }
    },
    scoper:add_meta(Meta).

set_otel_context(#{cowboy_req := Req}) ->
    Headers = cowboy_req:headers(Req),
    %% Implicitly puts OTEL context into process dictionary.
    %% Since cowboy does not reuse process for other requests, we don't care
    %% about cleaning it up.
    _OtelCtx = otel_propagator_text_map:extract(maps:to_list(Headers)),
    ok.
