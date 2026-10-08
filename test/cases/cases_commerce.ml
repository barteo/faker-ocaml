open T
module C = Faker.Commerce

let cases : case list =
  [
    ("department", fun f -> s (C.department f));
    ("productName", fun f -> s (C.product_name f));
    ("price", fun f -> s (C.price f));
    ("price/minmax", fun f -> s (C.price ~min:100.0 ~max:200.0 f));
    ("price/dec0", fun f -> s (C.price ~min:1.0 ~max:50.0 ~dec:0 f));
    ("price/dec3", fun f -> s (C.price ~min:0.5 ~max:2.5 ~dec:3 ~symbol:"$" f));
    ("price/dec1", fun f -> s (C.price ~min:10.0 ~max:11.0 ~dec:1 ~symbol:"€" f));
    ("price/small", fun f -> s (C.price ~min:0.0 ~max:1.0 ~dec:2 f));
    ("price/tight", fun f -> s (C.price ~min:5.01 ~max:5.03 ~dec:2 f));
    ("price/eq", fun f -> s (C.price ~min:7.5 ~max:7.5 ~dec:3 ~symbol:"£" f));
    ("price/neg", fun f -> s (C.price ~min:(-5.0) ~max:10.0 ~symbol:"$" f));
    ("price/big", fun f -> s (C.price ~min:1000.0 ~max:1000000.0 ~dec:4 f));
    ("price/err", fun f -> s (C.price ~min:10.0 ~max:5.0 f));
    ("productAdjective", fun f -> s (C.product_adjective f));
    ("productMaterial", fun f -> s (C.product_material f));
    ("product", fun f -> s (C.product f));
    ("productDescription", fun f -> s (C.product_description f));
    ("isbn", fun f -> s (C.isbn f));
    ("isbn/10", fun f -> s (C.isbn ~variant:`V10 f));
    ("isbn/13", fun f -> s (C.isbn ~variant:`V13 f));
    ("isbn/opts10", fun f -> s (C.isbn ~variant:`V10 ~separator:" " f));
    ("isbn/opts13", fun f -> s (C.isbn ~separator:"" f));
    ("upc", fun f -> s (C.upc f));
    ("upc/prefix", fun f -> s (C.upc ~prefix:"0123" f));
    ("upc/prefix11", fun f -> s (C.upc ~prefix:"01234567890" f));
    ("upc/errdigits", fun f -> s (C.upc ~prefix:"12a" f));
    ("upc/errlen", fun f -> s (C.upc ~prefix:"012345678901" f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{commerce.price({\"min\":3,\"max\":9,\"symbol\":\"$\"})}} {{commerce.isbn(10)}} {{commerce.productName}}"
             f) );
  ]
