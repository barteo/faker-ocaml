open T
module N = Faker.Number

let cases : case list =
  [
    ("int", fun f -> i (N.int f));
    ("int/10", fun f -> i (N.int ~max:10 f));
    ("int/min-max", fun f -> i (N.int ~min:(-50) ~max:50 f));
    ("int/multipleOf", fun f -> i (N.int ~min:10 ~max:1000 ~multiple_of:7 f));
    ("int/same", fun f -> i (N.int ~min:5 ~max:5 f));
    ("int/err", fun f -> i (N.int ~min:10 ~max:1 f));
    ("int/nosuitable", fun f -> i (N.int ~min:1 ~max:3 ~multiple_of:10 f));
    ( "int/exp",
      fun f ->
        i (N.int ~max:100 ~distributor:(Faker.Distributor.exponential ()) f) );
    ( "float/exp",
      fun f ->
        n
          (N.float ~max:100.0
             ~distributor:(Faker.Distributor.exponential ~base:5.0 ())
             f) );
    ("float", fun f -> n (N.float f));
    ("float/max", fun f -> n (N.float ~max:100.0 f));
    ("float/range", fun f -> n (N.float ~min:(-10.5) ~max:33.3 f));
    ("float/fd2", fun f -> n (N.float ~min:0.0 ~max:100.0 ~fraction_digits:2 f));
    ("float/fd0", fun f -> n (N.float ~max:1000.0 ~fraction_digits:0 f));
    ("float/fd5", fun f -> n (N.float ~min:1.5 ~max:2.5 ~fraction_digits:5 f));
    ("float/mult", fun f -> n (N.float ~min:0.0 ~max:10.0 ~multiple_of:0.25 f));
    ("float/mult3", fun f -> n (N.float ~min:0.0 ~max:100.0 ~multiple_of:3.0 f));
    ("float/err", fun f -> n (N.float ~min:2.0 ~max:1.0 f));
    ( "float/errMultFd",
      fun f -> n (N.float ~max:10.0 ~multiple_of:0.5 ~fraction_digits:2 f) );
    ("binary", fun f -> s (N.binary f));
    ("binary/255", fun f -> s (N.binary ~max:255 f));
    ("binary/range", fun f -> s (N.binary ~min:4 ~max:9 f));
    ("octal/default", fun f -> s (N.octal f));
    ("hex/255", fun f -> s (N.hex ~max:255 f));
    ("octal", fun f -> s (N.octal ~min:10 ~max:5000 f));
    ("hex", fun f -> s (N.hex f));
    ("hex/range", fun f -> s (N.hex ~min:0 ~max:65535 f));
    ("bigInt", fun f -> s (string_of_int (N.big_int f)));
    ("bigInt/100", fun f -> s (string_of_int (N.big_int ~max:100 f)));
    ( "bigInt/range",
      fun f -> s (string_of_int (N.big_int ~min:(-1000000) ~max:99999999999 f))
    );
    ( "bigInt/mult",
      fun f -> s (string_of_int (N.big_int ~min:5 ~max:500000 ~multiple_of:7 f))
    );
    ("romanNumeral", fun f -> s (N.roman_numeral f));
    ("romanNumeral/range", fun f -> s (N.roman_numeral ~min:1 ~max:20 f));
    ("romanNumeral/errMin", fun f -> s (N.roman_numeral ~min:0 ~max:5 f));
    ("romanNumeral/errMax", fun f -> s (N.roman_numeral ~min:1 ~max:4000 f));
  ]
