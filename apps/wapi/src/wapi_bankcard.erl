-module(wapi_bankcard).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").

-export([lookup_bank_info/2]).
-export([merge_data/3]).
-export([payment_system/1]).

-type bank_info() :: #{
    payment_system := bankcard_validator:payment_system(),
    bank_name := binary(),
    issuer_country := dmsl_domain_thrift:'CountryCode'() | undefined,
    metadata := map()
}.

-type lookup_error() ::
    notfound
    | {invalid,
        payment_system
        | issuer_country}.

-type card_data() :: cds_proto_storage_thrift:'PutCardData'().
-type extra_card_data() :: #{
    cardholder => binary() | undefined,
    exp_date => {integer(), integer()}
}.

-type session_data() :: cds_proto_storage_thrift:'SessionData'().

-spec lookup_bank_info(_PAN :: binary(), woody_context:ctx()) -> {ok, bank_info()} | {error, lookup_error()}.
lookup_bank_info(PAN, WoodyCtx) ->
    RequestVersion = {'last', #binbase_Last{}},
    case wapi_woody_client:call_service(binbase, 'Lookup', {PAN, RequestVersion}, WoodyCtx) of
        {ok, BinData} ->
            decode_bank_info(BinData);
        {exception, #binbase_BinNotFound{}} ->
            {error, notfound}
    end.

decode_bank_info(#binbase_ResponseData{bin_data = BinData, version = Version}) ->
    try
        {ok, #{
            payment_system => BinData#binbase_BinData.payment_system,
            bank_name => BinData#binbase_BinData.bank_name,
            issuer_country => decode_issuer_country(BinData#binbase_BinData.iso_country_code),
            metadata => #{
                <<"version">> => Version
            }
        }}
    catch
        {invalid, What} ->
            {error, {invalid, What}}
    end.

-define(invalid(What), erlang:throw({invalid, What})).

%% Residence mapping
%%
-spec decode_issuer_country(binary() | undefined) -> dmsl_domain_thrift:'Residence'() | undefined.
decode_issuer_country(Residence) when is_binary(Residence) ->
    try
        {enum, Variants} = dmsl_domain_thrift:enum_info('CountryCode'),
        Variant = erlang:list_to_existing_atom(string:to_lower(erlang:binary_to_list(Residence))),
        element(1, lists:keyfind(Variant, 1, Variants))
    catch
        error:badarg ->
            _ = logger:warning("unknown residence encountered: ~s", [Residence]),
            ?invalid(issuer_country)
    end;
decode_issuer_country(undefined) ->
    undefined.

-spec payment_system(bank_info()) -> bankcard_validator:payment_system().
payment_system(BankInfo) ->
    maps:get(payment_system, BankInfo).

-spec merge_data(card_data(), extra_card_data(), session_data() | undefined) -> bankcard_validator:bankcard_data().
merge_data(CardData, ExtraCardData, undefined) ->
    maps:merge(convert_card_data(CardData), ExtraCardData);
merge_data(CardData, ExtraCardData, #cds_SessionData{auth_data = AuthData}) ->
    CVC = get_cvc_from_session_data(AuthData),
    CardDataMap0 = convert_card_data(CardData),
    CardDataMap1 = maps:merge(CardDataMap0, ExtraCardData),
    CardDataMap1#{cvc => maybe_undefined(CVC)}.

get_cvc_from_session_data({card_security_code, AuthData}) ->
    AuthData#cds_CardSecurityCode.value;
get_cvc_from_session_data(_) ->
    undefined.

convert_card_data(CardData) ->
    #cds_PutCardData{
        pan = PAN
    } = CardData,
    #{
        card_number => PAN
    }.

maybe_undefined(<<>>) ->
    undefined;
maybe_undefined(CVC) ->
    CVC.
