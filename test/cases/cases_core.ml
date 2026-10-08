open T

let keys = function
  | J.Obj o -> J.Arr (Array.of_list (List.map (fun (k, _) -> s k) o))
  | _ -> J.Null

let member k o = Option.get (J.member k o)

let cases : case list =
  [
    ( "create/emptyLocale",
      fun _ ->
        ignore (Faker.create ~locale:[] ());
        J.Null );
    ( "create/defaultRefDate",
      fun f ->
        let seed = Faker.Number.int ~max:1000 f in
        let g =
          Faker.create ~seed
            ~default_ref_date:
              (Faker.Date_util.of_iso "2020-06-15T12:00:00.000Z")
            ()
        in
        date (Faker.Date.recent g) );
    ( "setDefaultRefDate/string",
      fun f ->
        Faker.set_default_ref_date_input f (`Str "2020-02-02T00:00:00.000Z");
        date (Faker.Date.past f) );
    ( "setDefaultRefDate/number",
      fun f ->
        Faker.set_default_ref_date_input f (`Num 1577836800000.0);
        date (Faker.Date.soon f) );
    ( "seed",
      fun f ->
        Faker.seed f 123;
        J.Arr [| i 123; i (Faker.Number.int f) |] );
    ( "simpleFaker",
      fun f ->
        let seed = Faker.Number.int ~max:1000 f in
        let g = Faker.create_simple ~seed () in
        let a = Faker.Number.int g in
        let b' = Faker.String.uuid g in
        let c = Faker.Datatype.boolean g in
        let d = Faker.Helpers.array_element [| "a"; "b" |] g in
        J.Arr [| i a; s b'; b c; s d |] );
    ( "mergeLocales",
      fun _ ->
        let m = Faker.merge_locales (Faker.Locales.De_AT.chain ()) in
        let first = member "female" (member "first_name" (member "person" m)) in
        let first =
          match first with J.Arr a -> J.Arr (Array.sub a 0 3) | v -> v
        in
        J.Arr [| keys m; keys (member "person" m); first |] );
    ( "definitions/missing",
      fun _ ->
        let g = Faker.create ~locale:[ Faker.Locales.base () ] () in
        Faker.definition g "person" "first_name" );
  ]
