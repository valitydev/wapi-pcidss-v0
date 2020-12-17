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

-export([redact/2]).
-export([mask_and_keep/4]).
-export([mask/4]).

-export([unwrap/1]).
-export([define/2]).

-export([get_path/2]).
-export([get_url/2]).
-export([get_url/3]).

-export([get_last_pan_digits/1]).
-export([decode_masked_pan/3]).

-export([get_unique_id/0]).

-type binding_value() :: binary().
-type url() :: binary().
-type path() :: binary().

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

-spec parse_lifetime(binary()) -> {ok, timeout()} | {error, bad_lifetime}.
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

unit_factor(<<"ms">>) ->
    {ok, 1};
unit_factor(<<"s">>) ->
    {ok, 1000};
unit_factor(<<"m">>) ->
    {ok, 1000 * 60};
unit_factor(_Other) ->
    {error, unknown_unit}.

-spec base64url_to_map(binary()) -> map() | no_return().
base64url_to_map(Base64) when is_binary(Base64) ->
    jsx:decode(base64url:decode(Base64), [return_maps]).

-spec map_to_base64url(map()) -> binary() | no_return().
map_to_base64url(Map) when is_map(Map) ->
    base64url:encode(jsx:encode(Map)).

-spec redact(Subject :: binary(), Pattern :: binary()) -> Redacted :: binary().
redact(Subject, Pattern) ->
    case re:run(Subject, Pattern, [global, {capture, all_but_first, index}]) of
        {match, Captures} ->
            lists:foldl(fun redact_match/2, Subject, Captures);
        nomatch ->
            Subject
    end.

redact_match({S, Len}, Subject) ->
    <<Pre:S/binary, _:Len/binary, Rest/binary>> = Subject,
    <<Pre/binary, (binary:copy(<<"*">>, Len))/binary, Rest/binary>>;
redact_match([Capture], Message) ->
    redact_match(Capture, Message).

%% TODO Switch to this sexy code after the upgrade to Erlang 20+
%%
%% -spec mask(leading|trailing, non_neg_integer(), char(), binary()) ->
%%     binary().
%% mask(Dir = trailing, MaskLen, MaskChar, Str) ->
%%     mask(Dir, 0, string:length(Str) - MaskLen, MaskChar, Str);
%% mask(Dir = leading, MaskLen, MaskChar, Str) ->
%%     mask(Dir, MaskLen, string:length(Str), MaskChar, Str).

%% mask(Dir, KeepStart, KeepLen, MaskChar, Str) ->
%%     unicode:characters_to_binary(string:pad(
%%         string:slice(Str, KeepStart, KeepLen),
%%         string:length(Str),
%%         Dir,
%%         MaskChar
%%     )).

-spec mask_and_keep(leading | trailing, non_neg_integer(), char(), binary()) -> binary().
mask_and_keep(trailing, KeepLen, MaskChar, Chardata) ->
    StrLen = erlang:length(unicode:characters_to_list(Chardata)),
    mask(leading, StrLen - KeepLen, MaskChar, Chardata);
mask_and_keep(leading, KeepLen, MaskChar, Chardata) ->
    StrLen = erlang:length(unicode:characters_to_list(Chardata)),
    mask(trailing, StrLen - KeepLen, MaskChar, Chardata).

-spec mask(leading | trailing, non_neg_integer(), char(), binary()) -> binary().
mask(trailing, MaskLen, MaskChar, Chardata) ->
    Str = unicode:characters_to_list(Chardata),
    unicode:characters_to_binary(
        string:left(string:substr(Str, 1, erlang:length(Str) - MaskLen), erlang:length(Str), MaskChar)
    );
mask(leading, MaskLen, MaskChar, Chardata) ->
    Str = unicode:characters_to_list(Chardata),
    unicode:characters_to_binary(
        string:right(string:substr(Str, MaskLen + 1), erlang:length(Str), MaskChar)
    ).

-spec unwrap(ok | {ok, Value} | {error, _Error}) -> Value | no_return().
unwrap(ok) ->
    ok;
unwrap({ok, Value}) ->
    Value;
unwrap({error, Error}) ->
    erlang:error({unwrap_error, Error}).

-spec define(undefined | T, T) -> T.
define(undefined, V) ->
    V;
define(V, _Default) ->
    V.

-spec get_path(wapi_handler_utils:route_match(), [binding_value()]) -> path().
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

-spec get_url(url(), path()) -> url().
get_url(BaseUrl, Path) ->
    <<BaseUrl/binary, Path/binary>>.

-spec get_url(url(), wapi_handler_utils:route_match(), [binding_value()]) -> url().
get_url(BaseUrl, PathSpec, Params) ->
    get_url(BaseUrl, get_path(PathSpec, Params)).

-define(MASKED_PAN_MAX_LENGTH, 4).

-spec get_last_pan_digits(binary()) -> binary().
get_last_pan_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
get_last_pan_digits(MaskedPan) ->
    MaskedPan.

-spec decode_masked_pan(pos_integer(), binary(), binary()) -> binary().
decode_masked_pan(PanLen, Bin, LastDigits) ->
    Mask = binary:copy(<<"*">>, PanLen - byte_size(Bin) - byte_size(LastDigits)),
    <<Bin/binary, Mask/binary, LastDigits/binary>>.

-spec get_unique_id() -> binary().
get_unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec redact_test() -> _.

redact_test() ->
    P1 = <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>,
    ?assertEqual(<<"+7******3210">>, redact(<<"+79876543210">>, P1)),
    ?assertEqual(<<"+1*11">>, redact(<<"+1111">>, P1)).

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

-spec mask_test() -> _.
mask_test() ->
    ?assertEqual(<<"Вий">>, mask(leading, 0, $*, <<"Вий">>)),
    ?assertEqual(<<"*ий">>, mask(leading, 1, $*, <<"Вий">>)),
    ?assertEqual(<<"**й">>, mask(leading, 2, $*, <<"Вий">>)),
    ?assertEqual(<<"***">>, mask(leading, 3, $*, <<"Вий">>)),
    ?assertEqual(<<"Вий">>, mask(trailing, 0, $*, <<"Вий">>)),
    ?assertEqual(<<"Ви*">>, mask(trailing, 1, $*, <<"Вий">>)),
    ?assertEqual(<<"В**">>, mask(trailing, 2, $*, <<"Вий">>)),
    ?assertEqual(<<"***">>, mask(trailing, 3, $*, <<"Вий">>)).

-spec mask_and_keep_test() -> _.
mask_and_keep_test() ->
    ?assertEqual(<<"***">>, mask_and_keep(leading, 0, $*, <<"Вий">>)),
    ?assertEqual(<<"В**">>, mask_and_keep(leading, 1, $*, <<"Вий">>)),
    ?assertEqual(<<"Ви*">>, mask_and_keep(leading, 2, $*, <<"Вий">>)),
    ?assertEqual(<<"Вий">>, mask_and_keep(leading, 3, $*, <<"Вий">>)),
    ?assertEqual(<<"***">>, mask_and_keep(trailing, 0, $*, <<"Вий">>)),
    ?assertEqual(<<"**й">>, mask_and_keep(trailing, 1, $*, <<"Вий">>)),
    ?assertEqual(<<"*ий">>, mask_and_keep(trailing, 2, $*, <<"Вий">>)),
    ?assertEqual(<<"Вий">>, mask_and_keep(trailing, 3, $*, <<"Вий">>)).

-spec parse_lifetime_test() -> _.
parse_lifetime_test() ->
    {ok, 16 * 1000} = parse_lifetime(<<"16s">>),
    {ok, 32 * 60 * 1000} = parse_lifetime(<<"32m">>),
    {error, bad_lifetime} = parse_lifetime(undefined),
    {error, bad_lifetime} = parse_lifetime(<<"64h">>).

-endif.
