(* Locale data access, mirroring src/internal/locale-proxy.ts. *)

let en = lazy (Json.parse En_data.json)
let base = lazy (Json.parse Base_data.json)

let missing path =
  Core.error
    "The locale data for '%s' are missing in this locale.\n\
    \  If this is a custom Faker instance, please make sure all required locales are used e.g. \
     '[de_AT, de, en, base]'.\n\
    \  Please contribute the missing data to the project or use a locale/Faker instance that has \
     these data.\n\
    \  For more information see https://fakerjs.dev/guide/localization.html"
    path

let not_applicable path =
  Core.error
    "The locale data for '%s' aren't applicable to this locale.\n\
    \  If you think this is a bug, please report it at: https://github.com/faker-js/faker"
    path

(** [get f category entry] is [faker.definitions.<category>.<entry>]. *)
let get (f : Core.t) category entry : Json.t =
  let cat = Json.member category f.locale in
  match Option.bind cat (Json.member entry) with
  | Some Json.Null -> not_applicable (category ^ "." ^ entry)
  | Some v -> v
  | None -> missing (category ^ "." ^ entry)

(** Like [get] but returns [None] when the entry is absent. *)
let find (f : Core.t) category entry : Json.t option =
  match Option.bind (Json.member category f.locale) (Json.member entry) with
  | Some Json.Null | None -> None
  | v -> v

(* Descends into nested objects by key. *)
let rec path (v : Json.t) (keys : string list) : Json.t option =
  match keys with
  | [] -> Some v
  | k :: rest -> Option.bind (Json.member k v) (fun v -> path v rest)

let to_string = function Json.Str s -> s | v -> Json.to_js_string v

let to_strings = function
  | Json.Arr a -> Array.map to_string a
  | v -> [| to_string v |]

(** [strings f category entry] for entries that are string arrays. *)
let strings f category entry = to_strings (get f category entry)

let num = function Json.Num n -> n | _ -> nan
