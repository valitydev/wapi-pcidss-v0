-module(wapi_auth).

-export([get_access_config/0]).
-export([authorize_api_key/3]).
-export([issue_access_token/2]).
-export([issue_access_token/3]).

-export([get_consumer/1]).

-export([get_resource_hierarchy/0]).
-export([get_operation_access/2]).

-define(DEFAULT_ACCESS_TOKEN_LIFETIME, 259200).
-define(DOMAIN, <<"common-api">>).

-define(SIGNEE, wapi).

-type context() :: uac:context().
-type claims() :: uac:claims().
-type consumer() :: client | merchant | provider.
-type request_data() :: #{atom() | binary() => term()}.

-export_type([context/0]).
-export_type([claims/0]).
-export_type([consumer/0]).

-type operation_id() :: wapi_handler:operation_id().

-type api_key() ::
    %% swag_wallet_server:api_key() |
    swag_server_payres:api_key()
    | swag_server_privdoc:api_key().

-type handler_opts() :: wapi_handler:opts().

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) ->
    %% | false.
    {true, context()}.
authorize_api_key(OperationID, ApiKey, _Opts) ->
    case uac:authorize_api_key(ApiKey, get_verification_options()) of
        {ok, Context} ->
            {true, Context};
        {error, Error} ->
            _ = log_auth_error(OperationID, Error),
            false
    end.

log_auth_error(OperationID, Error) ->
    logger:info("API Key authorization failed for ~p due to ~p", [OperationID, Error]).

%%

-type token_spec() ::
    {destinations, DestinationID :: binary()}.

-spec issue_access_token(wapi_handler_utils:party_id(), token_spec()) -> uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec) ->
    issue_access_token(PartyID, TokenSpec, #{}).

-spec issue_access_token(wapi_handler_utils:party_id(), token_spec(), map()) -> uac_authorizer_jwt:token().
issue_access_token(PartyID, TokenSpec, ExtraProperties) ->
    Claims0 = resolve_token_spec(TokenSpec),
    Claims = maps:merge(ExtraProperties, Claims0),
    wapi_utils:unwrap(
        uac_authorizer_jwt:issue(
            wapi_utils:get_unique_id(),
            PartyID,
            Claims,
            ?SIGNEE
        )
    ).

-spec resolve_token_spec(token_spec()) -> claims().
resolve_token_spec({destinations, DestinationId}) ->
    DomainRoles = #{
        <<"common-api">> => uac_acl:from_list([
            {[party, {destinations, DestinationId}], read},
            {[party, {destinations, DestinationId}], write}
        ])
    },
    Expiration = {lifetime, ?DEFAULT_ACCESS_TOKEN_LIFETIME},
    #{
        <<"exp">> => Expiration,
        <<"resource_access">> => DomainRoles
    }.

%%

-spec get_operation_access(operation_id(), request_data()) -> [{uac_acl:scope(), uac_acl:permission()}].
get_operation_access('StoreBankCard', _) ->
    [{[party], write}];
get_operation_access('GetBankCard', _) ->
    [{[party], read}];
get_operation_access('StorePrivateDocument', _) ->
    [{[party], write}].

-spec get_resource_hierarchy() -> #{atom() => map()}.
%% TODO add some sence in here
get_resource_hierarchy() ->
    #{
        party => #{
            wallets => #{},
            destinations => #{}
        }
    }.

-spec get_consumer(claims()) -> consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client">> -> client;
        <<"provider">> -> provider
    end.

-spec get_access_config() -> map().
get_access_config() ->
    #{
        domain_name => ?DOMAIN,
        resource_hierarchy => get_resource_hierarchy()
    }.

get_verification_options() ->
    #{
        domains_to_decode => [?DOMAIN]
    }.
