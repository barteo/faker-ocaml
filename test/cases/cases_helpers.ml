open T
module H = Faker.Helpers

let arr_ = [| "a"; "b"; "c"; "d"; "e"; "f"; "g" |]
let obj = [ ("a", 1); ("b", 2); ("c", 3) ]

let cases : case list =
  [
    ("arrayElement", fun f -> s (H.array_element arr_ f));
    ("arrayElement/one", fun f -> s (H.array_element [| "x" |] f));
    ("arrayElements", fun f -> ss (H.array_elements arr_ f));
    ("arrayElements/3", fun f -> ss (H.array_elements ~count:(`N 3) arr_ f));
    ("arrayElements/range", fun f -> ss (H.array_elements ~count:(`Range (2, 4)) arr_ f));
    ("arrayElements/all", fun f -> ss (H.array_elements ~count:(`N 10) arr_ f));
    ("arrayElements/0", fun f -> ss (H.array_elements ~count:(`N 0) arr_ f));
    ("shuffle/inplace", fun f -> ss (H.shuffle ~inplace:true (Array.copy arr_) f));
    ("shuffle", fun f -> ss (H.shuffle arr_ f));
    ( "weightedArrayElement",
      fun f -> s (H.weighted_array_element [| (5.0, "sunny"); (4.0, "rainy"); (1.0, "snowy") |] f) );
    ("uniqueArray", fun f -> ss (H.unique_array [| "a"; "a"; "b"; "c"; "c"; "d" |] 3 f));
    ("uniqueArray/fn", fun f -> is (H.unique_array_fn (fun () -> Faker.Number.int ~max:20 f) 5));
    ("multiple", fun f -> is (H.multiple (fun _ -> Faker.Number.int ~max:9 f) f));
    ( "multiple/range",
      fun f -> ss (H.multiple ~count:(`Range (1, 5)) (fun _ -> Faker.String.alpha ~length:(`N 2) f) f) );
    ("maybe", fun f -> opt s (H.maybe (fun () -> "yes") f));
    ("maybe/0.9", fun f -> opt s (H.maybe ~probability:0.9 (fun () -> "yes") f));
    ("objectKey", fun f -> s (H.object_key obj f));
    ("objectValue", fun f -> i (H.object_value obj f));
    ("objectEntry", fun f -> let k, v = H.object_entry obj f in J.Arr [| s k; i v |]);
    ( "enumValue/string",
      fun f -> s (H.enum_value [ ("Red", "red"); ("Green", "green"); ("Blue", "blue") ] f) );
    ( "enumValue/numeric",
      fun f ->
        H.enum_value
          [ ("0", s "Zero"); ("1", s "One"); ("2", s "Two"); ("Zero", i 0); ("One", i 1); ("Two", i 2) ]
          f );
    ( "enumValue/mixed",
      (* Entries in JS property order: integer keys first. *)
      fun f ->
        H.enum_value
          [ ("1", s "A"); ("A", i 1); ("B", s "b"); ("1e3", s "x"); ("0x1f", s "y"); (" ", s "z"); ("NaN", s "n") ]
          f );
    ("enumValue/empty", fun f -> H.enum_value [] f);
    ("rangeToNumber", fun f -> i (H.range_to_number (`Range (1, 100)) f));
    ( "slugify",
      fun _ ->
        ss
          (Array.map H.slugify
             [| " Hello World! "; "Ça va très bien"; "über_cool-ness.txt"; "a/b\\c"; "ﬁne"; "Ærøskøbing" |]) );
    ("replaceSymbols", fun f -> s (H.replace_symbols "#?*#?*-##??**" f));
    ("replaceCreditCardSymbols", fun f -> s (H.replace_credit_card_symbols "6453-####-####-####-###L" f));
    ("replaceCreditCardSymbols/pat", fun f -> s (H.replace_credit_card_symbols "1234-[4-9]-##!!-L" f));
    ("replaceCreditCardSymbols/sym", fun f -> s (H.replace_credit_card_symbols ~symbol:'*' "6011-*{4}-****-***L" f));
    ("fromRegExp/1", fun f -> s (H.from_reg_exp "#{5}" f));
    ("fromRegExp/2", fun f -> s (H.from_reg_exp "#{2,9}" f));
    ("fromRegExp/3", fun f -> s (H.from_reg_exp "[1-7]" f));
    ("fromRegExp/4", fun f -> s (H.from_reg_exp "#{3}test[1-5]" f));
    ("fromRegExp/5", fun f -> s (H.from_reg_exp "[0-9]{3}-[A-Z]{2,4}-.{5}" f));
    ("fromRegExp/6", fun f -> s (H.from_reg_exp ~flags:"" "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}" f));
    ("fromRegExp/7", fun f -> s (H.from_reg_exp ~flags:"i" "^[a-z]{3}$" f));
    ("fromRegExp/8", fun f -> s (H.from_reg_exp ~flags:"" "x?y*z+" f));
    ("fromRegExp/9", fun f -> s (H.from_reg_exp ~flags:"" "[^a-z]{4}" f));
    ("fromRegExp/10", fun f -> s (H.from_reg_exp ~flags:"i" "a.b.c" f));
    ("fromRegExp/11", fun f -> s (H.from_reg_exp "[-a-c]{6}" f));
    ("fromRegExp/12", fun f -> s (H.from_reg_exp ~flags:"i" "q" f));
    ("fromRegExp/13", fun f -> s (H.from_reg_exp "." f));
    ("fromRegExp/14", fun f -> s (H.from_reg_exp ~flags:"i" "[a-f0-9]{8}-[^0-9]{2}" f));
    ( "mustache",
      fun f ->
        s
          (H.mustache (Some "I found {{count}} instances of \"{{word}}\".")
             [ ("count", `F (fun _ -> string_of_int (Faker.Number.int ~max:9 f))); ("word", `S "th$is") ]) );
    ( "fake/registry",
      fun f ->
        s
          (H.fake
             "{{number.bigInt}} {{number.bigInt(10)}} {{number.bigInt({\"min\":5,\"max\":9})}} \
              {{helpers.objectEntry({\"a\":1,\"b\":2})}} {{helpers.enumValue({\"A\":\"x\",\"B\":\"y\"})}}"
             f) );
    ("fake/literal", fun f -> s (H.fake "no tokens here" f));
    ( "fake/number",
      fun f -> s (H.fake "{{number.int}} and {{number.int(5)}} and {{number.int({\"min\":10,\"max\":20})}}" f) );
    ("fake/string", fun f -> s (H.fake "{{string.alpha(5)}}-{{string.numeric({\"length\":3})}}-{{string.uuid}}" f));
    ("fake/helpers", fun f -> s (H.fake "{{helpers.arrayElement([\"a\",\"b\",\"c\"])}}{{helpers.fromRegExp(X{3})}}" f));
    ("fake/array", fun f -> s (H.fake_one_of [| "{{number.int(9)}}"; "x{{datatype.boolean}}" |] f));
    ("fake/unresolvable", fun f -> s (H.fake "{{foo.bar}}" f));
  ]
