-module(wapi_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
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
-export([store_bank_card_expired_test/1]).
-export([store_bank_card_invalid_cardholder_test/1]).
-export([get_bank_card_success_test/1]).
-export([store_privdoc_success_test/1]).
-export([store_pan_only_bank_card_ok_test/1]).

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [
            store_bank_card_success_test,
            store_bank_card_expired_test,
            store_bank_card_invalid_cardholder_test,
            store_pan_only_bank_card_ok_test,
            get_bank_card_success_test,
            store_privdoc_success_test
        ]}
    ].

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
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

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, C) ->
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec store_bank_card_success_test(config()) -> test_return().
store_bank_card_success_test(C) ->
    CardNumber = <<"4150399999000900">>,
    wapi_ct_helper:mock_services(
        [
            {binbase, fun('Lookup', _) ->
                {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)}
            end},
            {cds_storage, fun
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT(CardNumber)};
                ('PutSession', _Args) -> {ok, ?PUT_SESSION_RESULT}
            end}
        ],
        C
    ),
    Bin = ?BIN(CardNumber),
    LastDigits = ?LAST_DIGITS(CardNumber),
    {ok, #{
        <<"bin">> := Bin,
        <<"lastDigits">> := LastDigits,
        <<"paymentSystem">> := <<"visa">>
    }} = wapi_client_payres:store_bank_card(?config(context, C), ?STORE_BANK_CARD_REQUEST(CardNumber)).

-spec store_bank_card_expired_test(config()) -> test_return().
store_bank_card_expired_test(C) ->
    Ctx = ?config(context, C),
    _ = wapi_ct_helper:mock_services(
        [
            {binbase, fun('Lookup', _) ->
                {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)}
            end},
            {cds_storage, fun
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT(?PAN)};
                ('PutSession', _Args) -> {ok, ?PUT_SESSION_RESULT}
            end}
        ],
        C
    ),
    _ = ?assertMatch(
        {ok, #{}},
        wapi_client_payres:store_bank_card(Ctx, ?STORE_BANK_CARD_REQUEST(?PAN, ?EXP_DATE_NEARLY_EXPIRED))
    ),
    _ = ?assertMatch(
        {error, {422, #{}}},
        wapi_client_payres:store_bank_card(Ctx, ?STORE_BANK_CARD_REQUEST(?PAN, ?EXP_DATE_EXPIRED))
    ).

-spec store_bank_card_invalid_cardholder_test(config()) -> test_return().
store_bank_card_invalid_cardholder_test(C) ->
    wapi_ct_helper:mock_services(
        [
            {binbase, fun('Lookup', _) ->
                {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)}
            end},
            {cds_storage, fun
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT(<<"4150399999000900">>)};
                ('PutSession', _Args) -> {ok, ?PUT_SESSION_RESULT}
            end}
        ],
        C
    ),
    BankCard = #{
        <<"type">> => <<"BankCard">>,
        <<"cardNumber">> => <<"4150399999000900">>,
        <<"expDate">> => ?EXP_DATE,
        <<"cardHolder">> => ?STRING
    },
    {ok, _} = wapi_client_payres:store_bank_card(
        ?config(context, C),
        BankCard#{<<"cardHolder">> => <<"ЛЕХА СВОТИН"/utf8>>}
    ),
    {error, {request_validation_failed, _}} = wapi_client_payres:store_bank_card(
        ?config(context, C),
        BankCard#{<<"cardHolder">> => <<"4150399999000900">>}
    ),
    {error, {request_validation_failed, _}} = wapi_client_payres:store_bank_card(
        ?config(context, C),
        BankCard#{<<"cardHolder">> => <<"ЛЕХА 123"/utf8>>}
    ).

-spec store_pan_only_bank_card_ok_test(config()) -> test_return().
store_pan_only_bank_card_ok_test(C) ->
    CardNumber = <<"4150399999000900">>,
    wapi_ct_helper:mock_services(
        [
            {binbase, fun('Lookup', _) ->
                {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)}
            end},
            {cds_storage, fun
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT(CardNumber)};
                ('PutSession', _Args) -> {ok, ?PUT_SESSION_RESULT}
            end}
        ],
        C
    ),
    {ok, #{
        <<"bin">> := <<"415039">>,
        <<"lastDigits">> := <<"0900">>,
        <<"paymentSystem">> := <<"visa">>
    }} = wapi_client_payres:store_bank_card(?config(context, C), ?STORE_BANK_CARD_REQUEST(CardNumber)).

-spec get_bank_card_success_test(config()) -> test_return().
get_bank_card_success_test(C) ->
    CardNumber = <<"4150399999000900">>,
    wapi_ct_helper:mock_services(
        [
            {cds_storage, fun
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT(CardNumber)};
                ('PutSession', _) -> {ok, ?PUT_SESSION_RESULT}
            end},
            {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)} end}
        ],
        C
    ),
    {ok, #{
        <<"token">> := Token
    }} = wapi_client_payres:store_bank_card(?config(context, C), ?STORE_BANK_CARD_REQUEST(CardNumber)),
    Bin = ?BIN(CardNumber),
    LastDigits = ?LAST_DIGITS(CardNumber),
    {ok, #{
        <<"bin">> := Bin,
        <<"lastDigits">> := LastDigits,
        <<"token">> := Token,
        <<"paymentSystem">> := <<"visa">>
    }} = wapi_client_payres:get_bank_card(?config(context, C), Token).

-spec store_privdoc_success_test(config()) -> test_return().
store_privdoc_success_test(C) ->
    wapi_ct_helper:mock_services([{identdoc_storage, fun('Put', _) -> {ok, ?STRING} end}], C),
    {ok, _} = wapi_client_privdoc:store_private_document(?config(context, C), ?STORE_PRIVATE_DOCUMENT_REQUEST).
