-module(wapi_handler).

%% API
-export([handle_request/5]).
-export([throw_result/1]).

%% Behaviour definition

-type operation_id() ::
    swag_server_payres:operation_id().

-type swagger_context() ::
    swag_server_payres:request_context().

-type context() :: #{
    woody_context := woody_context:ctx(),
    swagger_context := swagger_context()
}.

-type opts() ::
    swag_server_payres:handler_opts(term()).

-type req_data() :: #{atom() | binary() => term()}.
-type status_code() :: 200..599.
-type headers() :: cowboy:http_headers().
-type response_data() :: map() | [map()] | undefined.
-type request_result() :: {ok | error, {status_code(), headers(), response_data()}}.

-callback process_request(operation_id(), req_data(), context(), opts()) -> request_result() | no_return().

-export_type([operation_id/0]).
-export_type([swagger_context/0]).
-export_type([context/0]).
-export_type([opts/0]).
-export_type([req_data/0]).
-export_type([status_code/0]).
-export_type([response_data/0]).
-export_type([headers/0]).
-export_type([request_result/0]).

%% API

-define(request_result, wapi_req_result).

-spec handle_request(operation_id(), req_data(), swagger_context(), module(), opts()) -> request_result().
handle_request(OperationID, Req, SwagContext = #{auth_context := AuthContext}, Handler, Opts) ->
    RpcID = create_rpc_id(Req),
    ok = set_rpc_meta(RpcID),
    ok = set_request_meta(Req),
    _ = logger:info("Processing request ~p", [OperationID]),
    try
        OperationACL = wapi_auth:get_operation_access(OperationID, Req),
        case uac:authorize_operation(OperationACL, AuthContext) of
            ok ->
                WoodyContext = create_woody_context(RpcID, AuthContext, Opts),
                Context = create_handler_context(SwagContext, WoodyContext),
                Handler:process_request(OperationID, Req, Context, Opts);
            {error, _} = Error ->
                _ = logger:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                wapi_handler_utils:reply_error(
                    401,
                    wapi_handler_utils:get_error_msg(<<"Unauthorized operation">>)
                )
        end
    catch
        throw:{?request_result, Result} ->
            Result;
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details);
        Class:Reason:Stacktrace ->
            process_general_error(Class, Reason, Stacktrace, Req, SwagContext)
    after
        ok = clear_rpc_meta()
    end.

-spec throw_result(request_result()) -> no_return().
throw_result(Res) ->
    erlang:throw({?request_result, Res}).

-spec create_rpc_id(req_data()) -> woody:rpc_id().
create_rpc_id(#{'X-Request-ID' := RequestID}) ->
    woody_context:new_rpc_id(genlib:to_binary(RequestID)).

-spec create_woody_context(req_data(), wapi_auth:context(), opts()) -> woody_context:ctx().
create_woody_context(RpcID, AuthContext, Opts) ->
    woody_user_identity:put(collect_user_identity(AuthContext, Opts), woody_context:new(RpcID)).

-spec set_rpc_meta(woody:rpc_id()) -> ok.
set_rpc_meta(RpcID) ->
    %% trace_id, parent_id and span_id must be top-level meta keys
    logger:update_process_metadata(maps:with([trace_id, parent_id, span_id], RpcID)).

-spec clear_rpc_meta() -> ok.
clear_rpc_meta() ->
    case logger:get_process_metadata() of
        undefined ->
            ok;
        Metadata ->
            logger:set_process_metadata(maps:without([trace_id, parent_id, span_id], Metadata))
    end.

-spec set_request_meta(req_data()) -> ok.
set_request_meta(#{'X-Request-ID' := RequestID}) ->
    scoper:add_meta(#{request_id => RequestID}).

-define(APP, wapi).

collect_user_identity(AuthContext, _Opts) ->
    genlib_map:compact(#{
        id => uac_authorizer_jwt:get_subject_id(AuthContext),
        %% TODO pass realm via Opts
        realm => genlib_app:env(?APP, realm),
        email => uac_authorizer_jwt:get_claim(<<"email">>, AuthContext, undefined),
        username => uac_authorizer_jwt:get_claim(<<"name">>, AuthContext, undefined)
    }).

-spec create_handler_context(swagger_context(), woody_context:ctx()) -> context().
create_handler_context(SwagContext, WoodyContext) ->
    #{
        woody_context => WoodyContext,
        swagger_context => SwagContext
    }.

process_woody_error(_Source, result_unexpected, _Details) -> wapi_handler_utils:reply_error(500);
process_woody_error(_Source, resource_unavailable, _Details) -> wapi_handler_utils:reply_error(503);
process_woody_error(_Source, result_unknown, _Details) -> wapi_handler_utils:reply_error(504).

process_general_error(Class, Reason, Stacktrace, Req, SwagContext) ->
    _ = logger:error(
        "Operation failed due to ~p:~p given req: ~p and context: ~p",
        [Class, Reason, Req, SwagContext],
        #{
            error => #{
                class => genlib:to_binary(Class),
                reason => genlib:format(Reason),
                stack_trace => genlib_format:format_stacktrace(Stacktrace)
            }
        }
    ),
    wapi_handler_utils:reply_error(500).
