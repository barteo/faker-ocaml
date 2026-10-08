open T
module S = Faker.String

let cases : case list =
  [
    ("fromCharacters", fun f -> s (S.from_characters "abc" f));
    ("fromCharacters/arr", fun f -> s (S.from_character_array ~length:(`N 5) [| "a"; "bb"; "c" |] f));
    ("fromCharacters/range", fun f -> s (S.from_characters ~length:(`Range (2, 8)) "xyz" f));
    ("fromCharacters/empty", fun f -> s (S.from_characters "" f));
    ("fromCharacters/emptyArr", fun f -> s (S.from_character_array ~length:(`N 3) [||] f));
    ("alpha", fun f -> s (S.alpha f));
    ("alpha/10", fun f -> s (S.alpha ~length:(`N 10) f));
    ("alpha/opts", fun f -> s (S.alpha ~length:(`Range (3, 12)) ~casing:`Upper ~exclude:[ "A"; "B" ] f));
    ("alpha/lower", fun f -> s (S.alpha ~length:(`N 7) ~casing:`Lower ~exclude:[ "x"; "y"; "z" ] f));
    ("alphanumeric", fun f -> s (S.alphanumeric f));
    ("alphanumeric/20", fun f -> s (S.alphanumeric ~length:(`N 20) f));
    ("alphanumeric/opts", fun f -> s (S.alphanumeric ~length:(`N 8) ~casing:`Lower ~exclude:[ "0"; "a" ] f));
    ("binary", fun f -> s (S.binary f));
    ("binary/opts", fun f -> s (S.binary ~length:(`N 12) ~prefix:"" f));
    ("octal", fun f -> s (S.octal ~length:(`Range (2, 6)) f));
    ("hexadecimal", fun f -> s (S.hexadecimal f));
    ("hexadecimal/opts", fun f -> s (S.hexadecimal ~length:(`N 16) ~casing:`Upper ~prefix:"#" f));
    ("hexadecimal/lower", fun f -> s (S.hexadecimal ~length:(`N 6) ~casing:`Lower f));
    ("hexadecimal/mixed", fun f -> s (S.hexadecimal ~length:(`N 12) ~casing:`Mixed ~prefix:"" f));
    ("numeric", fun f -> s (S.numeric f));
    ("numeric/12", fun f -> s (S.numeric ~length:(`N 12) f));
    ("numeric/opts", fun f -> s (S.numeric ~length:(`N 6) ~allow_leading_zeros:false ~exclude:[ "5" ] f));
    ("numeric/noleading", fun f -> s (S.numeric ~length:(`N 4) ~allow_leading_zeros:false f));
    ( "numeric/err",
      fun f -> s (S.numeric ~length:(`N 4) ~exclude:[ "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ] f) );
    ("sample", fun f -> s (S.sample f));
    ("sample/5", fun f -> s (S.sample ~length:(`N 5) f));
    ("sample/range", fun f -> s (S.sample ~length:(`Range (3, 6)) f));
    ("uuid", fun f -> s (S.uuid f));
    ("uuid/v7", fun f -> s (S.uuid ~version:`V7 f));
    ("uuid/v4", fun f -> s (S.uuid ~version:`V4 f));
    ( "uuid/v7ref",
      fun f -> s (S.uuid ~version:`V7 ~ref_date:(Faker.Date_util.of_iso "2020-02-03T04:05:06.789Z") f) );
    ("ulid", fun f -> s (S.ulid f));
    ("ulid/ref", fun f -> s (S.ulid ~ref_date:1600000000000.0 f));
    ("ulid/errNegative", fun f -> s (S.ulid ~ref_date:(-1.0) f));
    ("ulid/errTooLarge", fun f -> s (S.ulid ~ref_date:(2.0 ** 48.0) f));
    ("nanoid", fun f -> s (S.nanoid f));
    ("nanoid/5", fun f -> s (S.nanoid ~length:(`N 5) f));
    ("nanoid/range", fun f -> s (S.nanoid ~length:(`Range (5, 9)) f));
    ("symbol", fun f -> s (S.symbol f));
    ("symbol/10", fun f -> s (S.symbol ~length:(`N 10) f));
  ]
