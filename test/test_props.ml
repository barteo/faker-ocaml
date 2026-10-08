(* Invariants that must hold for any seed. *)

let iterations = 1000

let for_seeds name prop =
  Alcotest.test_case name `Quick (fun () ->
      for seed = 1 to iterations do
        let f = Faker.create ~seed () in
        if not (prop f) then Alcotest.failf "%s failed for seed %d" name seed
      done)

let all_chars p s = String.for_all p s
let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let () =
  Alcotest.run ~compact:true "faker properties"
    [
      ( "number",
        [
          for_seeds "int in range" (fun f ->
              let v = Faker.Number.int ~min:(-5) ~max:5 f in
              v >= -5 && v <= 5);
          for_seeds "int multipleOf" (fun f -> Faker.Number.int ~max:1000 ~multiple_of:7 f mod 7 = 0);
          for_seeds "float in range" (fun f ->
              let v = Faker.Number.float ~min:1.5 ~max:2.5 f in
              v >= 1.5 && v < 2.5);
          for_seeds "float fraction digits" (fun f ->
              let v = Faker.Number.float ~max:100.0 ~fraction_digits:2 f in
              let s = Faker.Js.number_to_string v in
              match String.index_opt s '.' with None -> true | Some i -> String.length s - i - 1 <= 2);
          for_seeds "bigInt in range" (fun f ->
              let v = Faker.Number.big_int ~min:10 ~max:20 f in
              v >= 10 && v <= 20);
          for_seeds "roman numeral" (fun f ->
              all_chars (fun c -> String.contains "MDCLXVI" c) (Faker.Number.roman_numeral f));
        ] );
      ( "string",
        [
          for_seeds "alpha length" (fun f -> String.length (Faker.String.alpha ~length:(`N 12) f) = 12);
          for_seeds "numeric no leading zero" (fun f ->
              (Faker.String.numeric ~length:(`N 5) ~allow_leading_zeros:false f).[0] <> '0');
          for_seeds "uuid v4 format" (fun f ->
              let u = Faker.String.uuid f in
              String.length u = 36 && u.[14] = '4'
              && String.contains "89ab" u.[19]
              && all_chars (fun c -> c = '-' || is_hex c) u);
          for_seeds "ulid length" (fun f -> String.length (Faker.String.ulid f) = 26);
          for_seeds "nanoid length" (fun f -> String.length (Faker.String.nanoid f) = 21);
        ] );
      ( "helpers",
        [
          for_seeds "shuffle is a permutation" (fun f ->
              let a = [| 1; 2; 3; 4; 5; 6 |] in
              let s = Array.copy (Faker.Helpers.shuffle a f) in
              Array.sort compare s;
              s = a);
          for_seeds "arrayElements subset" (fun f ->
              let a = [| "a"; "b"; "c"; "d" |] in
              Array.for_all (fun x -> Array.mem x a) (Faker.Helpers.array_elements a f));
          for_seeds "credit card luhn" (fun f ->
              Faker.Helpers.luhn_check (Faker.Helpers.replace_credit_card_symbols "6453-####-####-####-###L" f));
          for_seeds "fromRegExp digits" (fun f ->
              let s = Faker.Helpers.from_reg_exp "[0-9]{4}" f in
              String.length s = 4 && all_chars is_digit s);
        ] );
      ( "seeding",
        [
          Alcotest.test_case "same seed, same output" `Quick (fun () ->
              let a = Faker.create ~seed:99 () and b = Faker.create ~seed:99 () in
              Alcotest.(check string) "uuid" (Faker.String.uuid a) (Faker.String.uuid b));
          Alcotest.test_case "reseeding resets" `Quick (fun () ->
              let f = Faker.create () in
              Faker.seed f 5;
              let x = Faker.String.alpha ~length:(`N 10) f in
              Faker.seed f 5;
              Alcotest.(check string) "alpha" x (Faker.String.alpha ~length:(`N 10) f));
        ] );
    ]
