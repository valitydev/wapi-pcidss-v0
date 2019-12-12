-define(STRING, <<"TEST">>).
-define(PAN, <<"4242424242424242">>).
-define(CVV, 123).
-define(EXP_DATE, <<"01/21">>).

-define(BIN(CardNumber), string:slice(CardNumber, 0, 6)).

-define(LAST_DIGITS(CardNumber), string:slice(CardNumber, 12)).

-define(MASKED_PAN(CardNumber), <<(?BIN(CardNumber))/binary, <<"******">>/binary, (?LAST_DIGITS(CardNumber))/binary>>).


-define(BANK_CARD, ?BANK_CARD(?PAN)).

-define(BANK_CARD(CardNumber), #'domain_BankCard'{
    token = ?STRING,
    payment_system = visa,
    masked_pan = ?MASKED_PAN(CardNumber),
    bin = ?BIN(CardNumber)
}).

-define(PUT_CARD_DATA_RESULT, ?PUT_CARD_DATA_RESULT(?PAN)).

-define(PUT_CARD_DATA_RESULT(CardNumber), #'PutCardDataResult'{
    bank_card = ?BANK_CARD(CardNumber),
    session_id = ?STRING
}).

-define(CARD_DATA, ?CARD_DATA(?PAN)).

-define(CARD_DATA(CardNumber), #'CardData'{
    pan = CardNumber,
    exp_date = ?EXP_DATE,
    cardholder_name = ?STRING,
    cvv = ?CVV
}).

-define(STORE_BANK_CARD_REQUEST, ?STORE_BANK_CARD_REQUEST(?PAN)).

-define(STORE_BANK_CARD_REQUEST(CardNumber), #{
    <<"type">>       => <<"BankCard">>,
    <<"cardNumber">> => CardNumber,
    <<"expDate">>    => ?EXP_DATE,
    <<"cardHolder">> => ?STRING
}).
