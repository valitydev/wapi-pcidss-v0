-module(wapi_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("fistful_proto/include/fistful_fistful_base_thrift.hrl").
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
-export([store_bank_card_auth_failed_test/1]).
-export([store_bank_card_invalid_token_test/1]).
-export([store_bank_card_bin_not_found_test/1]).
-export([store_bank_card_invalid_card_data_test/1]).
-export([get_bank_card_not_found_test/1]).
-export([get_bank_card_success_test/1]).
-export([store_pan_only_bank_card_ok_test/1]).
-export([create_resource_test/1]).
-export([valid_until_resource_test/1]).

-type config() :: wapi_ct_helper:config().
-type test_case_name() :: atom().
-type group_name() :: atom().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default_auth},
        {group, custom_auth}
    ].

-spec groups() -> [{group_name(), [test_case_name()]}].
groups() ->
    [
        {default_auth, [
            store_bank_card_success_test,
            store_bank_card_expired_test,
            store_bank_card_invalid_cardholder_test,
            store_bank_card_bin_not_found_test,
            store_bank_card_invalid_card_data_test,
            get_bank_card_not_found_test,
            store_pan_only_bank_card_ok_test,
            get_bank_card_success_test,
            create_resource_test,
            valid_until_resource_test
        ]},
        {custom_auth, [
            store_bank_card_auth_failed_test,
            store_bank_card_invalid_token_test
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
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(default_auth, Config) ->
    SupPid = wapi_ct_helper:start_mocked_service_sup(?MODULE),
    _ = wapi_ct_helper_token_keeper:mock_user_session_token(SupPid),
    _ = wapi_ct_helper_bouncer:mock_arbiter(wapi_ct_helper_bouncer:judge_always_allowed(), SupPid),
    [{group_test_sup, SupPid}, {context, wapi_ct_helper:get_context(?API_TOKEN)} | Config];
init_per_group(_Name, Config) ->
    SupPid = wapi_ct_helper:start_mocked_service_sup(?MODULE),
    [{group_test_sup, SupPid}, {context, wapi_ct_helper:get_context(?API_TOKEN)} | Config].

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    _ = wapi_ct_helper:stop_mocked_service_sup(?config(group_test_sup, C)),
    _ = proplists:delete(context, C),
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    SupPid = wapi_ct_helper:start_mocked_service_sup(?MODULE),
    [{test_sup, SupPid} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    _ = wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec store_bank_card_success_test(config()) -> test_return().
store_bank_card_success_test(C) ->
    CardNumber = <<"4150399999000900">>,
    Bin = ?BIN(CardNumber),
    LastDigits = ?LAST_DIGITS(CardNumber),
    {ok, #{
        <<"bin">> := Bin,
        <<"lastDigits">> := LastDigits
    }} = call_store_bank_card(CardNumber, C).

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
    _ = wapi_ct_helper:mock_services(
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

-spec store_bank_card_bin_not_found_test(config()) -> test_return().
store_bank_card_bin_not_found_test(C) ->
    _ = wapi_ct_helper:mock_services(
        [
            {binbase, fun('Lookup', _) ->
                {throwing, #binbase_BinNotFound{}}
            end}
        ],
        C
    ),
    CardNumber = <<"4150399999000900">>,
    _ = ?assertMatch(
        {error, {422, #{<<"message">> := <<"Unsupported card">>}}},
        wapi_client_payres:store_bank_card(
            ?config(context, C),
            ?STORE_BANK_CARD_REQUEST(CardNumber)
        )
    ).

-spec store_bank_card_invalid_card_data_test(config()) -> test_return().
store_bank_card_invalid_card_data_test(C) ->
    _ = wapi_ct_helper:mock_services(
        [
            {binbase, fun('Lookup', _) ->
                {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)}
            end},
            {cds_storage, fun('PutCard', _) -> {throwing, #cds_InvalidCardData{}} end}
        ],
        C
    ),
    CardNumber = <<"4150399999000900">>,
    _ = ?assertMatch(
        {error, {422, #{<<"message">> := <<"Card data is invalid">>}}},
        wapi_client_payres:store_bank_card(
            ?config(context, C),
            ?STORE_BANK_CARD_REQUEST(CardNumber)
        )
    ).

-spec store_pan_only_bank_card_ok_test(config()) -> test_return().
store_pan_only_bank_card_ok_test(C) ->
    CardNumber = <<"4150399999000900">>,
    {ok, #{
        <<"bin">> := <<"415039">>,
        <<"lastDigits">> := <<"0900">>
    }} = call_store_bank_card(CardNumber, C).

-spec get_bank_card_success_test(config()) -> test_return().
get_bank_card_success_test(C) ->
    CardNumber = <<"4150399999000900">>,
    {ok, #{
        <<"token">> := Token
    }} = call_store_bank_card(CardNumber, C),
    Bin = ?BIN(CardNumber),
    LastDigits = ?LAST_DIGITS(CardNumber),
    {ok, #{
        <<"bin">> := Bin,
        <<"lastDigits">> := LastDigits,
        <<"token">> := Token
    }} = wapi_client_payres:get_bank_card(?config(context, C), Token).

-spec get_bank_card_not_found_test(config()) -> test_return().
get_bank_card_not_found_test(C) ->
    _ = ?assertMatch(
        {error, {404, #{}}},
        wapi_client_payres:get_bank_card(?config(context, C), <<"NOPE">>)
    ).

-spec create_resource_test(config()) -> test_return().
create_resource_test(C) ->
    CardNumber = <<"4150399999000900">>,
    {ok, #{
        <<"token">> := ResourceToken
    }} = call_store_bank_card(CardNumber, C),
    {ok, {{bank_card, BankCard}, _ValidUntil}} = wapi_crypto:decrypt_resource_token(ResourceToken),
    ?assertEqual(?BIN(CardNumber), BankCard#fistful_base_BankCard.bin),
    ?assertEqual(?LAST_DIGITS(CardNumber), BankCard#fistful_base_BankCard.masked_pan).

-spec valid_until_resource_test(config()) -> test_return().
valid_until_resource_test(C) ->
    CardNumber = <<"4150399999000900">>,
    {ok, #{
        <<"token">> := ResourceToken,
        <<"validUntil">> := ValidUntil
    }} = call_store_bank_card(CardNumber, C),
    {ok, {_Resource, DeadlineToken}} = wapi_crypto:decrypt_resource_token(ResourceToken),
    Deadline = wapi_utils:deadline_from_binary(ValidUntil),
    ?assertEqual(Deadline, DeadlineToken).

-spec store_bank_card_auth_failed_test(config()) -> test_return().
store_bank_card_auth_failed_test(C) ->
    _ = wapi_ct_helper_token_keeper:mock_user_session_token(?config(test_sup, C)),
    _ = wapi_ct_helper_bouncer:mock_arbiter(wapi_ct_helper_bouncer:judge_always_forbidden(), ?config(test_sup, C)),
    CardNumber = <<"4150399999000900">>,
    _ = ?assertMatch(
        {error, {401, #{}}},
        call_store_bank_card(CardNumber, C)
    ).

-spec store_bank_card_invalid_token_test(config()) -> test_return().
store_bank_card_invalid_token_test(C) ->
    _ = wapi_ct_helper_token_keeper:mock_invalid_token(?config(test_sup, C)),
    _ = wapi_ct_helper_bouncer:mock_arbiter(wapi_ct_helper_bouncer:judge_always_allowed(), ?config(test_sup, C)),
    CardNumber = <<"4150399999000900">>,
    _ = ?assertMatch(
        {error, {401, #{}}},
        call_store_bank_card(CardNumber, C)
    ).

%% Utils

call_store_bank_card(CardNumber, C) ->
    _ = wapi_ct_helper:mock_services(
        [
            {binbase, fun('Lookup', _) -> {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)} end},
            {cds_storage, fun
                ('PutCard', _) -> {ok, ?PUT_CARD_RESULT(CardNumber)};
                ('PutSession', _) -> {ok, ?PUT_SESSION_RESULT}
            end}
        ],
        C
    ),
    wapi_client_payres:store_bank_card(?config(context, C), ?STORE_BANK_CARD_REQUEST(CardNumber)).
