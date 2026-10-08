open T
module P = Faker.Phone

let cases : case list =
  [
    ("number", fun f -> s (P.number f));
    ("number/human", fun f -> s (P.number ~style:`Human f));
    ("number/national", fun f -> s (P.number ~style:`National f));
    ("number/international", fun f -> s (P.number ~style:`International f));
    ("number/mobile", fun f -> s (P.number ~style:`Mobile f));
    ("number/err", fun f -> s (P.number_with_style_name "nope" f));
    ("imei", fun f -> s (P.imei f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             {|{{phone.number}}|{{phone.number({"style":"national"})}}|{{phone.number({"style":"international"})}}|{{phone.imei}}|}
             f) );
  ]
