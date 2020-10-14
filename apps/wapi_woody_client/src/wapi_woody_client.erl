-module(wapi_woody_client).

-export([call_service/4]).
-export([call_service/5]).

-export([get_service_modname/1]).

%%
-define(APP, wapi_woody_client).

-type service_name() :: atom().

-spec call_service(service_name(), woody:func(), [term()], woody_context:ctx()) -> woody:result().
call_service(ServiceName, Function, Args, Context) ->
    EventHandlerOpts = genlib_app:env(wapi, scoper_event_handler_options, #{}),
    EventHandler = {scoper_woody_event_handler, EventHandlerOpts},
    call_service(ServiceName, Function, Args, Context, EventHandler).

-spec call_service(service_name(), woody:func(), [term()], woody_context:ctx(), woody:ev_handler()) -> woody:result().
call_service(ServiceName, Function, Args, Context, EventHandler) ->
    {Url, Service} = get_service_spec(ServiceName),
    Request = {Service, Function, Args},
    woody_client:call(Request, #{url => Url, event_handler => EventHandler}, Context).

get_service_spec(ServiceName) ->
    {get_service_url(ServiceName), get_service_modname(ServiceName)}.

get_service_url(ServiceName) ->
    maps:get(ServiceName, genlib_app:env(?APP, service_urls)).

-spec get_service_modname(service_name()) -> woody:service().
get_service_modname(binbase) ->
    {binbase_binbase_thrift, 'Binbase'};
get_service_modname(cds_storage) ->
    {cds_proto_storage_thrift, 'Storage'};
get_service_modname(identdoc_storage) ->
    {identdocstore_identity_document_storage_thrift, 'IdentityDocumentStorage'}.

%% get_service_modname(webhook_manager) ->
%%     {dmsl_webhooker_thrift, 'WebhookManager'}.
