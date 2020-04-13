-define(STRING, <<"TEST">>).
-define(INTEGER, 10000).
-define(TIMESTAMP, <<"2016-03-22T06:12:27Z">>).

-define(BANK_CARD, ?BANK_CARD(?PAN)).

-define(BANK_CARD(CardNumber), #cds_BankCard{
    token = ?STRING,
    last_digits = ?LAST_DIGITS(CardNumber),
    bin = ?BIN(CardNumber)
}).

-define(BINBASE_LOOKUP_RESULT, ?BINBASE_LOOKUP_RESULT(<<"MASTERCARD">>)).
-define(BINBASE_LOOKUP_RESULT(PaymentSystem), #'binbase_ResponseData'{
    bin_data = #'binbase_BinData' {
        payment_system = PaymentSystem,
        bank_name = ?STRING,
        iso_country_code = <<"KAZ">>,
        card_type = debit,
        bin_data_id = {i, 123}
    },
    version = ?INTEGER
}).

-define(PAN, <<"4242424242424242">>).
-define(CVV, 123).
-define(EXP_DATE, <<"01/21">>).
-define(EXP_DATE_NEARLY_EXPIRED, <<"02/20">>).
-define(EXP_DATE_EXPIRED, <<"01/20">>).
-define(DATE, <<"2016-03-22">>).

-define(BIN(CardNumber), string:slice(CardNumber, 0, 6)).

-define(LAST_DIGITS(CardNumber), string:slice(CardNumber, 12)).

-define(MASKED_PAN(CardNumber), <<(?BIN(CardNumber))/binary, <<"******">>/binary, (?LAST_DIGITS(CardNumber))/binary>>).

-define(PUT_CARD_RESULT, ?PUT_CARD_RESULT(?PAN)).

-define(PUT_CARD_RESULT(CardNumber), #cds_PutCardResult{
    bank_card = ?BANK_CARD(CardNumber)
}).

-define(PUT_SESSION_RESULT, ok).

-define(STORE_BANK_CARD_REQUEST, ?STORE_BANK_CARD_REQUEST(?PAN)).

-define(STORE_BANK_CARD_REQUEST(CardNumber),
    ?STORE_BANK_CARD_REQUEST(CardNumber, ?EXP_DATE)
).

-define(STORE_BANK_CARD_REQUEST(CardNumber, ExpDate), #{
    <<"type">>       => <<"BankCard">>,
    <<"cardNumber">> => CardNumber,
    <<"expDate">>    => ExpDate,
    <<"cardHolder">> => ?STRING
}).

-define(STORE_PRIVATE_DOCUMENT_REQUEST, #{
        <<"type">> => <<"RUSDomesticPassportData">>,
        <<"series">> => <<"1234">>,
        <<"number">> => <<"123456">>,
        <<"issuer">> => ?STRING,
        <<"issuerCode">> => <<"123-123">>,
        <<"issuedAt">> => ?DATE,
        <<"familyName">> => ?STRING,
        <<"firstName">> => ?STRING,
        <<"birthDate">> => ?DATE,
        <<"birthPlace">> => ?STRING
}).
