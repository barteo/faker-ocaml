(* Compares OCaml output against fixtures generated from faker-js
   (see tools/gen_fixtures.mjs). *)

let runs = 3
let ref_date = 1735689600000.0 (* 2025-01-01T00:00:00.000Z *)

let groups : (string * (string * int * string) list * (string * (Faker.t -> Faker.Json.t)) list) list =
  [ ("mersenne", Expected_mersenne.cases, Cases_mersenne.cases) ]
  @ Groups.groups

let run_case group fn seed expected =
  let runs = if group = "mersenne" then 1 else runs in
  let f = Faker.create ~seed () in
  Faker.set_default_ref_date f ref_date;
  let results =
    List.init runs (fun _ ->
        try fn f with Faker.Faker_error msg -> Faker.Json.Obj [ ("error", Faker.Json.Str msg) ])
  in
  let actual = Faker.Json.to_string (Faker.Json.Arr (Array.of_list results)) in
  Alcotest.(check string) "matches faker-js" expected actual

let () =
  let only = Sys.getenv_opt "ONLY" in
  let tests =
    List.filter_map
      (fun (group, expected, cases) ->
        if Option.fold ~none:false ~some:(fun o -> o <> group) only then None
        else
          let tbl = Hashtbl.create 64 in
          List.iter (fun (id, fn) -> Hashtbl.replace tbl id fn) cases;
          let tcs =
            List.map
              (fun (id, seed, json) ->
                let name = Printf.sprintf "%s seed=%d" id seed in
                match Hashtbl.find_opt tbl id with
                | Some fn -> Alcotest.test_case name `Quick (fun () -> run_case group fn seed json)
                | None -> Alcotest.test_case name `Quick (fun () -> Alcotest.skip ()))
              expected
          in
          Some (group, tcs))
      groups
  in
  Alcotest.run ~compact:true "faker parity" tests
