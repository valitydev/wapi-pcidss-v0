-module(wapi_swagger_server).

-export([child_spec/2]).

-define(APP, wapi).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).
-define(RANCH_REF, ?MODULE).

-spec child_spec(cowboy_router:routes(), #{atom() => module()}) -> supervisor:child_spec().
child_spec(AdditionalRoutes, LogicHandlers) ->
    {Transport, TransportOpts} = get_socket_transport(),
    CowboyOpts = get_cowboy_config(AdditionalRoutes, LogicHandlers),
    GsTimeout = genlib_app:env(?APP, graceful_shutdown_timeout, 5000),
    Protocol = cowboy_clear,
    cowboy_draining_server:child_spec(
        ?RANCH_REF,
        Transport,
        TransportOpts,
        Protocol,
        CowboyOpts,
        GsTimeout
    ).

get_socket_transport() ->
    {ok, IP} = inet:parse_address(genlib_app:env(?APP, ip, ?DEFAULT_IP_ADDR)),
    Port = genlib_app:env(?APP, port, ?DEFAULT_PORT),
    AcceptorsPool = genlib_app:env(?APP, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    {ranch_tcp, #{
        num_acceptors => AcceptorsPool,
        socket_opts => [{ip, IP}, {port, Port}]
    }}.

get_cowboy_config(AdditionalRoutes, LogicHandlers) ->
    Dispatch =
        cowboy_router:compile(
            squash_routes(
                AdditionalRoutes ++
                    swag_server_payres_router:get_paths(maps:get(payres, LogicHandlers))
            )
        ),
    CowboyOpts = #{
        env => #{
            dispatch => Dispatch,
            cors_policy => wapi_cors_policy
        },
        middlewares => [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
        ],
        stream_handlers => [cowboy_access_log_h, wapi_stream_h, cowboy_stream_h]
    },
    cowboy_access_log_h:set_extra_info_fun(
        mk_operation_id_getter(CowboyOpts),
        CowboyOpts
    ).

squash_routes(Routes) ->
    orddict:to_list(
        lists:foldl(
            fun({K, V}, D) -> orddict:update(K, fun(V0) -> V0 ++ V end, V, D) end,
            orddict:new(),
            Routes
        )
    ).

mk_operation_id_getter(#{env := Env}) ->
    fun(Req) ->
        get_operation_id(Req, Env)
    end.

%% Ensure that request has host and path required for
%% cowboy_router:execute/2.
%% NOTE: Be careful when upgrade cowboy in this project
%% because cowboy_router:execute/2 call can change.
get_operation_id(#{host := _Host, path := _Path} = Req, Env) ->
    case cowboy_router:execute(Req, Env) of
        {ok, _, #{handler_opts := {_Operations, _LogicHandler, _SwaggerHandlerOpts} = HandlerOpts}} ->
            case swag_server_payres_utils:get_operation_id(Req, HandlerOpts) of
                undefined ->
                    #{};
                OperationID ->
                    #{operation_id => OperationID}
            end;
        _ ->
            #{}
    end;
get_operation_id(_Req, _Env) ->
    #{}.
