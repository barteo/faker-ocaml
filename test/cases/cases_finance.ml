open T
module F = Faker.Finance

let many ?(count = 40) fn f =
  ss (Faker.Helpers.multiple ~count:(`N count) (fun _ -> fn f) f)

let currency (c : F.currency) =
  J.Obj
    [
      ("name", s c.name);
      ("code", s c.code);
      ("symbol", s c.symbol);
      ("numericCode", s c.numeric_code);
    ]

let cases : case list =
  [
    ("accountNumber", fun f -> s (F.account_number f));
    ("accountNumber/5", fun f -> s (F.account_number ~length:5 f));
    ("accountNumber/opts", fun f -> s (F.account_number ~length:12 f));
    ("accountName", fun f -> s (F.account_name f));
    ("routingNumber", fun f -> s (F.routing_number f));
    ("routingNumber/many", many F.routing_number);
    ("amount", fun f -> s (F.amount f));
    ( "amount/opts",
      fun f -> s (F.amount ~min:5.0 ~max:10.0 ~dec:0 ~symbol:"$" f) );
    ("amount/dec4", fun f -> s (F.amount ~min:(-1000.0) ~max:1000.0 ~dec:4 f));
    ( "amount/auto",
      fun f -> s (F.amount ~min:1000.0 ~max:100000000.0 ~auto_format:true f) );
    ( "amount/auto0",
      fun f ->
        s
          (F.amount ~min:100.0 ~max:10000000.0 ~dec:0 ~auto_format:true
             ~symbol:"€" f) );
    ( "amount/auto5",
      fun f ->
        s (F.amount ~min:(-9999999.0) ~max:9999999.0 ~dec:5 ~auto_format:true f)
    );
    ("amount/autosmall", fun f -> s (F.amount ~max:999.0 ~auto_format:true f));
    ( "amount/autoneg",
      fun f ->
        s (F.amount ~min:(-5000.0) ~max:(-1000.0) ~dec:1 ~auto_format:true f) );
    ("amount/err", fun f -> s (F.amount ~min:10.0 ~max:1.0 f));
    ("amount/errdec", fun f -> s (F.amount ~dec:(-1) f));
    ("transactionType", fun f -> s (F.transaction_type f));
    ("currency", fun f -> currency (F.currency f));
    ("currencyCode", fun f -> s (F.currency_code f));
    ("currencyName", fun f -> s (F.currency_name f));
    ("currencySymbol", fun f -> s (F.currency_symbol f));
    ("currencySymbol/many", many ~count:20 F.currency_symbol);
    ("currencyNumericCode", fun f -> s (F.currency_numeric_code f));
    ("bitcoinAddress", fun f -> s (F.bitcoin_address f));
    ("bitcoinAddress/many", many ~count:12 (fun f -> F.bitcoin_address f));
    ("bitcoinAddress/legacy", fun f -> s (F.bitcoin_address ~type_:`Legacy f));
    ( "bitcoinAddress/segwit",
      fun f -> s (F.bitcoin_address ~type_:`Segwit ~network:`Testnet f) );
    ("bitcoinAddress/bech32", fun f -> s (F.bitcoin_address ~type_:`Bech32 f));
    ( "bitcoinAddress/bech32t",
      fun f -> s (F.bitcoin_address ~type_:`Bech32 ~network:`Testnet f) );
    ( "bitcoinAddress/taproot",
      fun f -> s (F.bitcoin_address ~type_:`Taproot ~network:`Mainnet f) );
    ( "bitcoinAddress/testnet",
      fun f -> s (F.bitcoin_address ~network:`Testnet f) );
    ("litecoinAddress", fun f -> s (F.litecoin_address f));
    ("creditCardNumber", fun f -> s (F.credit_card_number f));
    ("creditCardNumber/many", many ~count:20 (fun f -> F.credit_card_number f));
    ("creditCardNumber/visa", fun f -> s (F.credit_card_number ~issuer:"visa" f));
    ( "creditCardNumber/Mastercard",
      fun f -> s (F.credit_card_number ~issuer:"MasterCard" f) );
    ("creditCardNumber/jcb", fun f -> s (F.credit_card_number ~issuer:"jcb" f));
    ( "creditCardNumber/diners",
      fun f -> s (F.credit_card_number ~issuer:"diners_club" f) );
    ( "creditCardNumber/amex",
      fun f -> s (F.credit_card_number ~issuer:"american_express" f) );
    ( "creditCardNumber/custom",
      fun f -> s (F.credit_card_number ~issuer:"63[7-9]#-####-####-###L" f) );
    ( "creditCardNumber/slash",
      fun f -> s (F.credit_card_number ~issuer:"1234/####/####L" f) );
    ( "creditCardNumber/unknown",
      fun f -> s (F.credit_card_number ~issuer:"foo" f) );
    ("creditCardCVV", fun f -> s (F.credit_card_cvv f));
    ("creditCardIssuer", fun f -> s (F.credit_card_issuer f));
    ("pin", fun f -> s (F.pin f));
    ("pin/6", fun f -> s (F.pin ~length:6 f));
    ("pin/opts", fun f -> s (F.pin ~length:1 f));
    ("pin/err", fun f -> s (F.pin ~length:0 f));
    ("ethereumAddress", fun f -> s (F.ethereum_address f));
    ("iban", fun f -> s (F.iban f));
    ("iban/many", many ~count:60 (fun f -> F.iban f));
    ("iban/formatted", fun f -> s (F.iban ~formatted:true f));
    ("iban/DE", fun f -> s (F.iban ~country_code:"DE" f));
    ("iban/GB", fun f -> s (F.iban ~country_code:"GB" ~formatted:true f));
    ("iban/FR", fun f -> s (F.iban ~country_code:"FR" f));
    ("iban/MT", fun f -> s (F.iban ~country_code:"MT" ~formatted:true f));
    ("iban/BR", fun f -> s (F.iban ~country_code:"BR" f));
    ("iban/empty", fun f -> s (F.iban ~country_code:"" f));
    ("iban/err", fun f -> s (F.iban ~country_code:"XX" f));
    ("bic", fun f -> s (F.bic f));
    ("bic/branch", fun f -> s (F.bic ~include_branch_code:true f));
    ("bic/nobranch", fun f -> s (F.bic ~include_branch_code:false f));
    ("transactionDescription", fun f -> s (F.transaction_description f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{finance.amount({\"min\":1,\"max\":5,\"symbol\":\"$\"})}} \
              {{finance.iban}} {{finance.pin(6)}} \
              {{finance.creditCardNumber(\"visa\")}}"
             f) );
    ( "fake/currency",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{finance.currency.code}} {{finance.currencyName}}" f) );
  ]
