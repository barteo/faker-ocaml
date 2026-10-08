(* The locale sweeps: tools/cases/sweep.mjs ids are "module.method", called with default options;
   tools/cases/l10n.mjs ids are "module.method(json args)". Both go through the helpers.fake
   registry, once per locale. *)

module J = Faker.Json

(* getMetadata() returns the raw (merged) metadata object; check the typed view agrees. *)
let metadata f =
  let raw =
    Option.value (J.member "metadata" (Faker.definitions f)) ~default:(J.Obj [])
  in
  let m = Faker.get_metadata f in
  let field k =
    match J.member k raw with Some (J.Str s) -> Some s | _ -> None
  in
  let dir = Option.map (function `Ltr -> "ltr" | `Rtl -> "rtl") m.dir in
  if
    (m.title, m.code, m.country, m.language, m.endonym, dir, m.script, m.variant)
    <> ( field "title",
         field "code",
         field "country",
         field "language",
         field "endonym",
         field "dir",
         field "script",
         field "variant" )
  then failwith "get_metadata disagrees with the raw metadata";
  raw

let find id =
  if id = "faker.getMetadata" then Some metadata
  else
    let call, args =
      match String.index_opt id '(' with
      | None -> (id, [])
      | Some p -> (
          let inner = String.sub id (p + 1) (String.length id - p - 2) in
          ( String.sub id 0 p,
            match J.parse ("[" ^ inner ^ "]") with
            | J.Arr a -> Array.to_list a
            | _ -> [] ))
    in
    match String.index_opt call '.' with
    | None -> None
    | Some i ->
        let m = String.sub call 0 i
        and name = String.sub call (i + 1) (String.length call - i - 1) in
        Option.map (fun fn f -> fn f args) (Faker.Registry.find_method m name)
