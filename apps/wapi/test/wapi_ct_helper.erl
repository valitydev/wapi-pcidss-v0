-module(wapi_ct_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("wapi_dummy_data.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([init_suite/2]).
-export([init_suite/3]).
-export([start_app/1]).
-export([start_app/2]).
-export([start_wapi/1]).
-export([start_wapi/2]).
-export([issue_token/2]).
-export([issue_token/3]).
-export([get_context/1]).
-export([get_context/2]).
-export([get_keysource/2]).
-export([start_mocked_service_sup/1]).
-export([stop_mocked_service_sup/1]).
-export([mock_services/2]).
-export([mock_services_/2]).

-define(WAPI_IP, "::").
-define(WAPI_PORT, 8080).
-define(WAPI_HOST_NAME, "localhost").
-define(WAPI_URL, ?WAPI_HOST_NAME ++ ":" ++ integer_to_list(?WAPI_PORT)).
-define(PAYMENT_SYSTEM_REF(ID), {payment_system, #domain_PaymentSystemRef{id = ID}}).
-define(PAYMENT_SYSTEM_OBJ(ID, Rules),
    {payment_system, #domain_PaymentSystemObject{
        ref = #domain_PaymentSystemRef{id = ID},
        data = #domain_PaymentSystem{
            name = ID,
            validation_rules = Rules
        }
    }}
).

%%
-type config() :: [{atom(), any()}].
-type app_name() :: atom().
-type context() :: #{
    url := string(),
    token := term(),
    timeout := integer(),
    event_handler := event_handler(),
    protocol := protocol(),
    deadline := iolist() | undefined,
    extra_properties := map()
}.

-export_type([config/0]).
-export_type([context/0]).

-type protocol() :: ipv4 | ipv6.

-export_type([protocol/0]).

-type event_handler() :: fun((event_type(), code(), duration()) -> ok).

-export_type([event_handler/0]).

-type event_type() :: atom().
-type code() :: pos_integer().
-type duration() :: non_neg_integer().

-spec init_suite(module(), config()) -> config().
init_suite(Module, Config) ->
    init_suite(Module, Config, []).

-spec init_suite(module(), config(), any()) -> config().
init_suite(Module, Config, WapiEnv) ->
    SupPid = start_mocked_service_sup(Module),
    Apps1 =
        start_app(woody) ++
            start_app(scoper),
    ServiceURLs = mock_services_(
        [
            {
                'Repository',
                {dmsl_domain_config_thrift, 'Repository'},
                fun('Checkout', _) ->
                    {ok, #'Snapshot'{
                        version = 1,
                        domain = #{
                            ?PAYMENT_SYSTEM_REF(<<"VISA">>) =>
                                ?PAYMENT_SYSTEM_OBJ(
                                    <<"VISA">>,
                                    bankcard_validator_legacy:get_payment_system_ruleset(<<"VISA">>)
                                ),
                            ?PAYMENT_SYSTEM_REF(<<"MASTERCARD">>) =>
                                ?PAYMENT_SYSTEM_OBJ(
                                    <<"MASTERCARD">>,
                                    bankcard_validator_legacy:get_payment_system_ruleset(<<"MASTERCARD">>)
                                )
                        }
                    }}
                end
            }
        ],
        SupPid
    ),
    Apps2 =
        start_wapi(Config, WapiEnv) ++
            start_app(dmt_client, [{max_cache_size, #{}}, {service_urls, ServiceURLs}, {cache_update_interval, 50000}]),
    [{apps, lists:reverse(Apps2 ++ Apps1)}, {suite_test_sup, SupPid} | Config].

-spec start_app(app_name()) -> [app_name()].
start_app(woody = AppName) ->
    start_app(AppName, [
        {acceptors_pool_size, 4}
    ]);
start_app(scoper = AppName) ->
    start_app(AppName, [
        {storage, scoper_storage_logger}
    ]);
start_app(AppName) ->
    genlib_app:start_application(AppName).

-spec start_app(app_name(), list()) -> [app_name()].
start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec start_wapi(config()) -> [app_name()].
start_wapi(Config) ->
    start_wapi(Config, []).

-spec start_wapi(config(), list()) -> [app_name()].
start_wapi(Config, ExtraEnv) ->
    JwkPublSource = {json, {file, get_keysource("keys/local/jwk.publ.json", Config)}},
    JwkPrivSource = {json, {file, get_keysource("keys/local/jwk.priv.json", Config)}},
    WapiEnv =
        ExtraEnv ++
            [
                {ip, ?WAPI_IP},
                {port, ?WAPI_PORT},
                {realm, <<"external">>},
                {public_endpoint, <<"localhost:8080">>},
                {access_conf, #{
                    jwt => #{
                        keyset => #{
                            wapi => {pem_file, get_keysource("keys/local/private.pem", Config)}
                        }
                    }
                }},
                {lechiffre_opts, #{
                    encryption_source => JwkPublSource,
                    decryption_sources => [JwkPrivSource]
                }},
                {validation, #{
                    env => #{now => {{2020, 02, 02}, {0, 0, 0}}}
                }},
                {payment_tool_token_lifetime, <<"1024s">>}
            ],
    start_app(wapi, WapiEnv).

-spec get_keysource(_, config()) -> _.
get_keysource(Key, Config) ->
    filename:join(?config(data_dir, Config), Key).

-spec start_mocked_service_sup(module()) -> pid().
start_mocked_service_sup(Module) ->
    {ok, SupPid} = supervisor:start_link(Module, []),
    _ = unlink(SupPid),
    SupPid.

-spec stop_mocked_service_sup(pid()) -> _.
stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

-spec issue_token(_, _) ->
    {ok, binary()}
    | {error, nonexistent_signee}.
issue_token(ACL, LifeTime) ->
    issue_token(?STRING, ACL, LifeTime).

-spec issue_token(_, _, _) ->
    {ok, binary()}
    | {error, nonexistent_signee}.
issue_token(PartyID, ACL, LifeTime) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list(ACL)
    },
    Claims = #{
        ?STRING => ?STRING,
        <<"exp">> => LifeTime,
        <<"resource_access">> => DomainRoles
    },
    uac_authorizer_jwt:issue(
        wapi_utils:get_unique_id(),
        PartyID,
        Claims,
        wapi
    ).

-spec get_context(binary()) -> wapi_client_lib:context().
get_context(Token) ->
    get_context(Token, #{}).

-spec get_context(binary(), map()) -> wapi_client_lib:context().
get_context(Token, ExtraProperties) ->
    wapi_client_lib:get_context(?WAPI_URL, Token, 10000, ipv4, ExtraProperties).

-spec mock_services(_, _) -> _.
mock_services(Services, SupOrConfig) ->
    start_woody_client(mock_services_(Services, SupOrConfig)).

start_woody_client(ServiceURLs) ->
    start_app(wapi_woody_client, [{service_urls, ServiceURLs}]).

-spec mock_services_(_, _) -> _.
mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));
mock_services_(Services, SupPid) when is_pid(SupPid) ->
    Name = lists:map(fun get_service_name/1, Services),
    Port = get_random_port(),
    {ok, IP} = inet:parse_address(?WAPI_IP),
    ChildSpec = woody_server:child_spec(
        {dummy, Name},
        #{
            ip => IP,
            port => Port,
            event_handler => {scoper_woody_event_handler, #{}},
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    lists:foldl(
        fun(Service, Acc) ->
            ServiceName = get_service_name(Service),
            Acc#{ServiceName => make_url(ServiceName, Port)}
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, wapi_woody_client:get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {wapi_dummy_service, #{function => Fun}}}}.

get_random_port() ->
    rand:uniform(32768) + 32767.

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?WAPI_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).
