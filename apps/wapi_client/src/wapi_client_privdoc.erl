-module(wapi_client_privdoc).

-export([store_private_document/2]).

-type context() :: wapi_client_lib:context().

-spec store_private_document(context(), map()) -> {ok, #{binary() => _}} | {error, term()}.

store_private_document(Context, Document) ->
    Params = #{body => Document},
    {Url, PreparedParams, Opts} = wapi_client_lib:make_request(Context, Params),
    Response = swag_client_privdoc_private_documents_api:store_private_document(Url, PreparedParams, Opts),
    wapi_client_lib:handle_response(Response).
