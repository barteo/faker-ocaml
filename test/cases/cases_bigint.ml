(* Bigint arithmetic and conversions; the lists mirror tools/cases/bigint.mjs. *)
open T
module B = Faker.Bigint

let operands =
  [
    "0";
    "1";
    "-1";
    "7";
    "-7";
    "9999";
    "-10000";
    "123456789012345678901234567890";
    "-98765432109876543210";
    "4611686018427387903";
    "-4611686018427387904";
    "4611686018427387904";
    "100000000000000000000000000000000000000000";
  ]

let ops =
  [
    ("add", B.add);
    ("sub", B.sub);
    ("mul", B.mul);
    ("div", B.div);
    ("rem", B.rem);
    ("cmp", fun a b -> B.of_int (B.compare a b));
  ]

let strings =
  [
    "";
    " ";
    "  12 ";
    "\u{a0}12\u{feff}";
    "\n7\t";
    "+5";
    "-5";
    "-0";
    "007";
    "0x1F";
    "0X1f";
    "0x00ff";
    "0o17";
    "0b101";
    "-0x10";
    "1e3";
    "1_000";
    "0x";
    "0b2";
    "-";
    "+";
    "12abc";
    "abc";
    "99999999999999999999999999999999999999";
    "-0000000000000000000000001";
    "0xffffffffffffffffffffffffffffffff";
  ]

let floats =
  [
    0.0;
    -0.0;
    1.0;
    -1.0;
    42.0;
    1e21;
    -1e21;
    0x1p53 +. 2.0;
    0x1p62;
    -0x1p62;
    0x1p63;
    1.7976931348623157e308;
    -1e300;
    1.5;
    -0.5;
    5e-324;
    Float.nan;
    Float.infinity;
    Float.neg_infinity;
  ]

let big v = s (B.to_string v)

let cases : case list =
  List.concat_map
    (fun (op, fn) ->
      List.concat_map
        (fun a ->
          List.filter_map
            (fun b ->
              if (op = "div" || op = "rem") && b = "0" then None
              else
                Some
                  ( Printf.sprintf "%s/%s/%s" op a b,
                    fun _ -> big (fn (B.of_string a) (B.of_string b)) ))
            operands)
        operands)
    ops
  @ List.map
      (fun a -> ("neg/" ^ a, fun _ -> big (B.neg (B.of_string a))))
      operands
  @ List.map
      (fun a ->
        ( "to_int/" ^ a,
          fun _ ->
            opt (fun n -> s (string_of_int n)) (B.to_int_opt (B.of_string a)) ))
      operands
  @ [
      ("of_int/min", fun _ -> big (B.of_int min_int));
      ("of_int/max", fun _ -> big (B.of_int max_int));
    ]
  @ List.mapi
      (fun i x ->
        (Printf.sprintf "of_string/%d" i, fun _ -> big (B.of_string x)))
      strings
  @ List.mapi
      (fun i x -> (Printf.sprintf "of_float/%d" i, fun _ -> big (B.of_float x)))
      floats
  @ [
      ("of_bool/true", fun _ -> big (B.of_bool true));
      ("of_bool/false", fun _ -> big (B.of_bool false));
    ]
