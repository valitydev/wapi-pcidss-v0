-module(wapi_handler_utils).

-export([get_error_msg/1]).

-export([reply_ok/1]).
-export([reply_ok/2]).
-export([reply_ok/3]).

-export([reply_error/1]).
-export([reply_error/2]).
-export([reply_error/3]).

-define(APP, wapi).

-type error_message() :: binary() | io_lib:chars().

-type status_code() :: wapi_handler:status_code().
-type headers() :: wapi_handler:headers().
-type response_data() :: wapi_handler:response_data().

%% API

-spec get_error_msg(error_message()) -> response_data().
get_error_msg(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

-spec reply_ok(status_code()) -> {ok, {status_code(), #{}, undefined}}.
reply_ok(Code) ->
    reply_ok(Code, undefined).

-spec reply_ok(status_code(), response_data()) -> {ok, {status_code(), #{}, response_data()}}.
reply_ok(Code, Data) ->
    reply_ok(Code, Data, #{}).

-spec reply_ok(status_code(), response_data(), headers()) -> {ok, {status_code(), #{}, response_data()}}.
reply_ok(Code, Data, Headers) ->
    reply(ok, Code, Data, Headers).

-spec reply_error(status_code()) -> {error, {status_code(), #{}, undefined}}.
reply_error(Code) ->
    reply_error(Code, undefined).

-spec reply_error(status_code(), response_data()) -> {error, {status_code(), #{}, response_data()}}.
reply_error(Code, Data) ->
    reply_error(Code, Data, #{}).

-spec reply_error(status_code(), response_data(), headers()) -> {error, {status_code(), #{}, response_data()}}.
reply_error(Code, Data, Headers) ->
    reply(error, Code, Data, Headers).

reply(Status, Code, Data, Headers) ->
    {Status, {Code, Headers, Data}}.
