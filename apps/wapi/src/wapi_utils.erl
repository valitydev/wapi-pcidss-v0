-module(wapi_utils).

-type deadline() :: woody:deadline().

-export_type([deadline/0]).

-export([deadline_to_binary/1]).
-export([deadline_from_binary/1]).
-export([deadline_from_timeout/1]).
-export([deadline_is_reached/1]).
-export([parse_lifetime/1]).

-export([base64url_to_map/1]).
-export([map_to_base64url/1]).

-export([get_path/2]).

-export([get_last_pan_digits/1]).

-export([parse_deadline/1]).

-type binding_value() :: binary().
-type path() :: binary().
% cowoby_router:route_match()
-type route_match() :: '_' | iodata().

%% API

-spec deadline_to_binary(deadline()) -> binary() | undefined.
deadline_to_binary(undefined) ->
    undefined;
deadline_to_binary(Deadline) ->
    woody_deadline:to_binary(Deadline).

-spec deadline_from_binary(binary()) -> deadline() | undefined.
deadline_from_binary(undefined) ->
    undefined;
deadline_from_binary(Binary) ->
    woody_deadline:from_binary(Binary).

-spec deadline_from_timeout(timeout()) -> deadline().
deadline_from_timeout(Timeout) ->
    woody_deadline:from_timeout(Timeout).

-spec deadline_is_reached(deadline()) -> boolean().
deadline_is_reached(Deadline) ->
    woody_deadline:is_reached(Deadline).

-spec parse_lifetime
    (undefined) -> {error, bad_lifetime};
    (binary()) -> {ok, timeout()} | {error, bad_lifetime}.
parse_lifetime(undefined) ->
    {error, bad_lifetime};
parse_lifetime(Bin) ->
    %% lifetime string like '1ms', '30s', '2.6m' etc
    %% default unit - millisecond
    case re:split(Bin, <<"^(\\d+\\.\\d+|\\d+)([a-z]*)$">>) of
        [<<>>, NumberStr, <<>>, <<>>] ->
            {ok, genlib:to_int(NumberStr)};
        [<<>>, NumberStr, Unit, <<>>] ->
            Number = genlib:to_float(NumberStr),
            case unit_factor(Unit) of
                {ok, Factor} ->
                    {ok, erlang:round(Number * Factor)};
                {error, _Reason} ->
                    {error, bad_lifetime}
            end;
        _Other ->
            {error, bad_lifetime}
    end.

-spec base64url_to_map(binary()) -> map() | no_return().
base64url_to_map(Base64) when is_binary(Base64) ->
    try
        {ok, Json} = jose_base64url:decode(Base64),
        jsx:decode(Json, [return_maps])
    catch
        Class:Reason ->
            _ = logger:debug("decoding base64 ~p to map failed with ~p:~p", [Base64, Class, Reason]),
            erlang:error(badarg)
    end.

-spec map_to_base64url(map()) -> binary() | no_return().
map_to_base64url(Map) when is_map(Map) ->
    try
        jose_base64url:encode(jsx:encode(Map))
    catch
        Class:Reason ->
            _ = logger:debug("encoding map ~p to base64 failed with ~p:~p", [Map, Class, Reason]),
            erlang:error(badarg)
    end.

%% TODO Switch to this sexy code after the upgrade to Erlang 20+
%%
%% -spec mask(leading|trailing, non_neg_integer(), char(), binary()) ->
%%     binary().
%% mask(Dir = trailing, MaskLen, MaskChar, Str) ->
%%     mask(Dir, 0, string:length(Str) - MaskLen, MaskChar, Str);
%% mask(Dir = leading, MaskLen, MaskChar, Str) ->
%%     mask(Dir, MaskLen, string:length(Str), MaskChar, Str).

%% mask(Dir, KeepStart, KeepLen, MaskChar, Str) ->
%%     unicode:characters_to_binary(
%%         string:pad(string:slice(Str, KeepStart, KeepLen), string:length(Str), Dir, MaskChar)
%%     ).

-spec to_universal_time(Timestamp :: binary()) -> TimestampUTC :: binary().
to_universal_time(Timestamp) ->
    TimestampMS = genlib_rfc3339:parse(Timestamp, microsecond),
    genlib_rfc3339:format_relaxed(TimestampMS, microsecond).

-spec get_path(route_match(), [binding_value()]) -> path().
get_path(PathSpec, Params) when is_list(PathSpec) ->
    get_path(genlib:to_binary(PathSpec), Params);
get_path(Path, []) ->
    Path;
get_path(PathSpec, [Value | Rest]) ->
    [P1, P2] = split(PathSpec),
    P3 = get_next(P2),
    get_path(<<P1/binary, Value/binary, P3/binary>>, Rest).

split(PathSpec) ->
    case binary:split(PathSpec, <<":">>) of
        Res = [_, _] -> Res;
        [_] -> erlang:error(param_mismatch)
    end.

get_next(PathSpec) ->
    case binary:split(PathSpec, <<"/">>) of
        [_, Next] -> <<"/", Next/binary>>;
        [_] -> <<>>
    end.

-define(MASKED_PAN_MAX_LENGTH, 4).

-spec get_last_pan_digits(binary()) -> binary().
get_last_pan_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
get_last_pan_digits(MaskedPan) ->
    MaskedPan.

-spec parse_deadline
    (binary()) -> {ok, woody:deadline()} | {error, bad_deadline};
    (undefined) -> {ok, undefined}.
parse_deadline(undefined) ->
    {ok, undefined};
parse_deadline(DeadlineStr) ->
    Parsers = [
        fun try_parse_woody_default/1,
        fun try_parse_relative/1
    ],
    try_parse_deadline(DeadlineStr, Parsers).

%%
%% Internals
%%
try_parse_deadline(_DeadlineStr, []) ->
    {error, bad_deadline};
try_parse_deadline(DeadlineStr, [P | Parsers]) ->
    case P(DeadlineStr) of
        {ok, _Deadline} = Result ->
            Result;
        {error, bad_deadline} ->
            try_parse_deadline(DeadlineStr, Parsers)
    end.

try_parse_woody_default(DeadlineStr) ->
    try
        Deadline = woody_deadline:from_binary(to_universal_time(DeadlineStr)),
        NewDeadline = clamp_max_request_deadline(woody_deadline:to_timeout(Deadline)),
        {ok, woody_deadline:from_timeout(NewDeadline)}
    catch
        error:{bad_deadline, _Reason} ->
            {error, bad_deadline};
        error:{badmatch, _} ->
            {error, bad_deadline};
        error:deadline_reached ->
            {error, bad_deadline}
    end.

try_parse_relative(DeadlineStr) ->
    %% deadline string like '1ms', '30m', '2.6h' etc
    case re:split(DeadlineStr, <<"^(\\d+\\.\\d+|\\d+)([a-z]+)$">>) of
        [<<>>, NumberStr, Unit, <<>>] ->
            Number = genlib:to_float(NumberStr),
            try_parse_relative(Number, Unit);
        _Other ->
            {error, bad_deadline}
    end.

try_parse_relative(Number, Unit) ->
    case unit_factor(Unit) of
        {ok, Factor} ->
            Timeout = erlang:round(Number * Factor),
            {ok, woody_deadline:from_timeout(clamp_max_request_deadline(Timeout))};
        {error, _Reason} ->
            {error, bad_deadline}
    end.

unit_factor(<<"ms">>) ->
    {ok, 1};
unit_factor(<<"s">>) ->
    {ok, 1000};
unit_factor(<<"m">>) ->
    {ok, 1000 * 60};
unit_factor(_Other) ->
    {error, unknown_unit}.

% 1 min
-define(MAX_REQUEST_DEADLINE_TIME, timer:minutes(1)).

clamp_max_request_deadline(Value) when is_integer(Value) ->
    MaxDeadline = genlib_app:env(wapi, max_request_deadline, ?MAX_REQUEST_DEADLINE_TIME),
    case Value > MaxDeadline of
        true ->
            MaxDeadline;
        false ->
            Value
    end.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_universal_time_test() -> _.

to_universal_time_test() ->
    ?assertEqual(<<"2017-04-19T13:56:07Z">>, to_universal_time(<<"2017-04-19T13:56:07Z">>)),
    ?assertEqual(<<"2017-04-19T13:56:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53Z">>)),
    ?assertEqual(<<"2017-04-19T10:36:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53+03:20">>)),
    ?assertEqual(<<"2017-04-19T17:16:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53-03:20">>)).

-spec get_path_test() -> _.
get_path_test() ->
    ?assertEqual(
        <<"/wallet/v0/deposits/11/events/42">>,
        get_path(
            <<"/wallet/v0/deposits/:depositID/events/:eventID">>,
            [<<"11">>, <<"42">>]
        )
    ),
    ?assertEqual(
        <<"/wallet/v0/deposits/11/events/42">>,
        get_path(
            "/wallet/v0/deposits/:depositID/events/:eventID",
            [<<"11">>, <<"42">>]
        )
    ),
    ?assertError(
        param_mismatch,
        get_path(
            "/wallet/v0/deposits/:depositID/events/:eventID",
            [<<"11">>, <<"42">>, <<"0">>]
        )
    ).

-spec parse_deadline_test() -> _.
parse_deadline_test() ->
    Deadline = woody_deadline:from_timeout(3000),
    BinDeadline = woody_deadline:to_binary(Deadline),
    {ok, {_, _}} = parse_deadline(BinDeadline),
    ?assertEqual({error, bad_deadline}, parse_deadline(<<"2017-04-19T13:56:07.53Z">>)),
    {ok, {_, _}} = parse_deadline(<<"15s">>),
    {ok, {_, _}} = parse_deadline(<<"15m">>),
    {error, bad_deadline} = parse_deadline(<<"15h">>).

-spec parse_lifetime_test() -> _.
parse_lifetime_test() ->
    {ok, 16 * 1000} = parse_lifetime(<<"16s">>),
    {ok, 32 * 60 * 1000} = parse_lifetime(<<"32m">>),
    {error, bad_lifetime} = parse_lifetime(undefined),
    {error, bad_lifetime} = parse_lifetime(<<"64h">>).

-endif.
