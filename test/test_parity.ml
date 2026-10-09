(* Compares OCaml output against fixtures generated from faker-js
   (see tools/gen_fixtures.mjs). *)

let runs = 3
let ref_date = 1735689600000.0 (* 2025-01-01T00:00:00.000Z *)

type group = {
  name : string;
  locale : string;
  expected : (string * int * string) list;
  find : string -> (Faker.t -> Faker.Json.t) option;
}

let of_list cases =
  let tbl = Hashtbl.create 64 in
  List.iter (fun (id, fn) -> Hashtbl.replace tbl id fn) cases;
  Hashtbl.find_opt tbl

let groups =
  {
    name = "mersenne";
    locale = Expected_mersenne.locale;
    expected = Expected_mersenne.cases;
    find = of_list Cases_mersenne.cases;
  }
  :: List.map
       (fun (name, locale, expected, cases) ->
         { name; locale; expected; find = of_list cases })
       Groups.groups
  @ List.map
      (fun (name, locale, expected) ->
        { name; locale; expected; find = Cases_sweep.find })
      (Expected_sweep.groups @ Expected_l10n.groups)

(* Each locale chain is merged once. *)
let merged = Hashtbl.create 16

let definitions code =
  match Hashtbl.find_opt merged code with
  | Some d -> d
  | None ->
      let chain =
        match Faker.All_locales.find_chain code with
        | Some c -> c
        | None -> failwith ("unknown locale " ^ code)
      in
      let d = Faker.merge_locales chain in
      Hashtbl.replace merged code d;
      d

(* Cases whose output goes through libm [pow] with a fractional exponent (the
   exponential distributor). Node's [Math.pow] calls the platform libm too, so
   faker-js itself can differ in the last bit across platforms (FreeBSD's msun
   vs the glibc/macOS libm the fixtures were generated with). These cases
   compare numbers with a relative tolerance instead of exactly. *)
let libm_pow_cases = [ ("number", "int/exp"); ("number", "float/exp") ]

let rec approx_equal (e : Faker.Json.t) (a : Faker.Json.t) =
  match (e, a) with
  | Num x, Num y -> Float.abs (x -. y) <= 1e-12 *. Float.max 1.0 (Float.abs x)
  | Arr xs, Arr ys ->
      Array.length xs = Array.length ys && Array.for_all2 approx_equal xs ys
  | Obj xs, Obj ys ->
      List.length xs = List.length ys
      && List.for_all2 (fun (k, x) (l, y) -> k = l && approx_equal x y) xs ys
  | _ -> e = a

let run_case group fn seed expected id =
  (* Case files may override the number of calls (tools/gen_fixtures.mjs `runs`). *)
  let runs =
    match Faker.Json.parse expected with
    | Faker.Json.Arr a -> Array.length a
    | _ -> runs
  in
  let f = Faker.create ~locale:[ definitions group.locale ] ~seed () in
  Faker.set_default_ref_date f ref_date;
  let results =
    List.init runs (fun _ ->
        try fn f
        with Faker.Faker_error msg ->
          Faker.Json.Obj [ ("error", Faker.Json.Str msg) ])
  in
  let actual = Faker.Json.to_string (Faker.Json.Arr (Array.of_list results)) in
  if
    expected <> actual
    && List.mem (group.name, id) libm_pow_cases
    && approx_equal (Faker.Json.parse expected) (Faker.Json.parse actual)
  then ()
  else Alcotest.(check string) "matches faker-js" expected actual

(* ONLY=person runs one group; ONLY=sweep_* runs every group with that prefix. *)
let selected name =
  match Sys.getenv_opt "ONLY" with
  | None -> true
  | Some o when String.ends_with ~suffix:"*" o ->
      String.starts_with ~prefix:(String.sub o 0 (String.length o - 1)) name
  | Some o -> o = name

let () =
  let tests =
    List.filter_map
      (fun group ->
        if not (selected group.name) then None
        else
          let tcs =
            List.map
              (fun (id, seed, json) ->
                let name = Printf.sprintf "%s seed=%d" id seed in
                match group.find id with
                | Some fn ->
                    Alcotest.test_case name `Quick (fun () ->
                        run_case group fn seed json id)
                | None ->
                    Alcotest.test_case name `Quick (fun () ->
                        Alcotest.fail
                          ("no OCaml case (or registry entry) for " ^ id)))
              group.expected
          in
          Some (group.name, tcs))
      groups
  in
  Alcotest.run ~compact:true "faker parity" tests
