(* Invariants that must hold for any seed. *)

let iterations = 1000

let for_seeds name prop =
  Alcotest.test_case name `Quick (fun () ->
      for seed = 1 to iterations do
        let f = Faker.create ~seed () in
        if not (prop f) then Alcotest.failf "%s failed for seed %d" name seed
      done)

(* Invariants that must hold in every locale (fewer seeds per locale). *)
let locale_seeds = 30

let for_locales name prop =
  Alcotest.test_case name `Quick (fun () ->
      List.iter
        (fun (code, chain) ->
          let locale = [ Faker.merge_locales (chain ()) ] in
          for seed = 1 to locale_seeds do
            let f = Faker.create ~locale ~seed () in
            (* Missing locale data raises Faker_error, like faker-js; nothing to check then. *)
            if not (try prop f with Faker.Faker_error _ -> true) then
              Alcotest.failf "%s failed for locale %s, seed %d" name code seed
          done)
        Faker.All_locales.all_chains)

(* Every registry method, with default options: only Faker_error may escape. *)
let registry_methods =
  let l = ref [] in
  Hashtbl.iter
    (fun m tbl ->
      Hashtbl.iter
        (fun name fn -> if m <> "helpers" then l := (m ^ "." ^ name, fn) :: !l)
        tbl)
    Faker.Registry.modules;
  List.sort compare !l

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
          for_seeds "int multipleOf" (fun f ->
              Faker.Number.int ~max:1000 ~multiple_of:7 f mod 7 = 0);
          for_seeds "float in range" (fun f ->
              let v = Faker.Number.float ~min:1.5 ~max:2.5 f in
              v >= 1.5 && v < 2.5);
          for_seeds "float fraction digits" (fun f ->
              let v = Faker.Number.float ~max:100.0 ~fraction_digits:2 f in
              let s = Faker.Js.number_to_string v in
              match String.index_opt s '.' with
              | None -> true
              | Some i -> String.length s - i - 1 <= 2);
          for_seeds "bigInt in range" (fun f ->
              let min = Faker.Bigint.of_int 10
              and max = Faker.Bigint.of_int 20 in
              let v = Faker.Number.big_int ~min ~max f in
              Faker.Bigint.compare v min >= 0 && Faker.Bigint.compare v max <= 0);
          for_seeds "bigInt in a wide range" (fun f ->
              let min =
                Faker.Bigint.of_string "-1000000000000000000000000000000"
              and max =
                Faker.Bigint.of_string "5000000000000000000000000000000"
              in
              let v = Faker.Number.big_int ~min ~max f in
              Faker.Bigint.compare v min >= 0 && Faker.Bigint.compare v max <= 0);
          for_seeds "roman numeral" (fun f ->
              all_chars
                (fun c -> String.contains "MDCLXVI" c)
                (Faker.Number.roman_numeral f));
        ] );
      ( "string",
        [
          for_seeds "alpha length" (fun f ->
              String.length (Faker.String.alpha ~length:(`N 12) f) = 12);
          for_seeds "numeric no leading zero" (fun f ->
              (Faker.String.numeric ~length:(`N 5) ~allow_leading_zeros:false f).[
              0]
              <> '0');
          for_seeds "uuid v4 format" (fun f ->
              let u = Faker.String.uuid f in
              String.length u = 36
              && u.[14] = '4'
              && String.contains "89ab" u.[19]
              && all_chars (fun c -> c = '-' || is_hex c) u);
          for_seeds "ulid length" (fun f ->
              String.length (Faker.String.ulid f) = 26);
          for_seeds "nanoid length" (fun f ->
              String.length (Faker.String.nanoid f) = 21);
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
              Array.for_all
                (fun x -> Array.mem x a)
                (Faker.Helpers.array_elements a f));
          for_seeds "credit card luhn" (fun f ->
              Faker.Helpers.luhn_check
                (Faker.Helpers.replace_credit_card_symbols
                   "6453-####-####-####-###L" f));
          for_seeds "fromRegExp digits" (fun f ->
              let s = Faker.Helpers.from_reg_exp "[0-9]{4}" f in
              String.length s = 4 && all_chars is_digit s);
        ] );
      ( "locales",
        [
          for_locales "methods only raise Faker_error" (fun f ->
              List.for_all
                (fun (name, fn) ->
                  match fn f [] with
                  | _ -> true
                  | exception Faker.Faker_error _ -> true
                  | exception e ->
                      Alcotest.failf "%s raised %s" name (Printexc.to_string e))
                registry_methods);
          for_locales "email local part" (fun f ->
              match String.split_on_char '@' (Faker.Internet.email f) with
              | [ local; domain ] ->
                  local <> "" && domain <> ""
                  && all_chars
                       (fun c ->
                         (c >= 'a' && c <= 'z')
                         || (c >= 'A' && c <= 'Z')
                         || is_digit c || String.contains "._+-" c)
                       local
              | _ -> false);
          for_locales "username is ASCII" (fun f ->
              all_chars (fun c -> Char.code c < 128) (Faker.Internet.username f));
          for_locales "slug is word characters" (fun f ->
              all_chars
                (fun c ->
                  (c >= 'a' && c <= 'z')
                  || (c >= 'A' && c <= 'Z')
                  || is_digit c || String.contains "_.-" c)
                (Faker.Lorem.slug f));
          for_locales "sentence ends with a period" (fun f ->
              String.ends_with ~suffix:"." (Faker.Lorem.sentence f));
        ] );
      ( "seeding",
        [
          Alcotest.test_case "same seed, same output" `Quick (fun () ->
              let a = Faker.create ~seed:99 ()
              and b = Faker.create ~seed:99 () in
              Alcotest.(check string)
                "uuid" (Faker.String.uuid a) (Faker.String.uuid b));
          Alcotest.test_case "reseeding resets" `Quick (fun () ->
              let f = Faker.create () in
              Faker.seed f 5;
              let x = Faker.String.alpha ~length:(`N 10) f in
              Faker.seed f 5;
              Alcotest.(check string)
                "alpha" x
                (Faker.String.alpha ~length:(`N 10) f));
        ] );
    ]
