-module(wapi_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("wapi_dummy_data.hrl").

-export([init/1]).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-behaviour(supervisor).

-export([store_bank_card_success_test/1]).
-export([get_bank_card_success_test  /1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [
            store_bank_card_success_test,
            get_bank_card_success_test
        ]}
    ].

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    wapi_ct_helper:init_suite(?MODULE, C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    _ = wapi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().

init_per_group(default, Config) ->
    Token = wapi_ct_helper:issue_token([{[party], write}, {[party], read}], unlimited),
    [{context, wapi_ct_helper:get_context(Token)} | Config].

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_Group, C) ->
    proplists:delete(context, C),
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.    

%%% Tests

-spec store_bank_card_success_test(config()) -> test_return().

store_bank_card_success_test(C) ->
    CardNumber = <<"4150399999000900">>,
    wapi_ct_helper:mock_services([{cds_storage, fun
        ('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT(CardNumber)}
    end}], C),
    Bin        = ?BIN(CardNumber),
    LastDigits = ?LAST_DIGITS(CardNumber),
    {ok, #{
        <<"bin">>        := Bin,
        <<"lastDigits">> := LastDigits
    }} = wapi_client_payres:store_bank_card(?config(context, C), ?STORE_BANK_CARD_REQUEST(CardNumber)).

-spec get_bank_card_success_test(config()) -> test_return().

get_bank_card_success_test(C) ->
    CardNumber = <<"4150399999000900">>,
     wapi_ct_helper:mock_services([{cds_storage, fun
        ('PutCardData', _) -> {ok, ?PUT_CARD_DATA_RESULT(CardNumber)};
        ('GetCardData', _) -> {ok, ?CARD_DATA(CardNumber)}
    end}], C),
    {ok, #{
        <<"token">> := Token
    }} = wapi_client_payres:store_bank_card(?config(context, C), ?STORE_BANK_CARD_REQUEST(CardNumber)),
    Bin        = ?BIN(CardNumber),
    LastDigits = ?LAST_DIGITS(CardNumber),
    {ok, #{
        <<"bin">>        := Bin,
        <<"lastDigits">> := LastDigits,
        <<"token">>      := Token
    }} = wapi_client_payres:get_bank_card(?config(context, C), Token).
