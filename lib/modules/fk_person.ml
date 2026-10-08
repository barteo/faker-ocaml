(* Port of src/modules/person/module.ts. *)

(** [SexType]: 'female' | 'generic' | 'male'. Functions taking [?sex] accept any
    subset, so a [Types.sex] value can be passed directly. *)
type sex_type = [ `Female | `Generic | `Male ]

let sex_type_to_string : [< sex_type ] -> string = function
  | `Female -> "female"
  | `Generic -> "generic"
  | `Male -> "male"

let sex_type_of_string = function
  | "female" -> Some `Female
  | "generic" -> Some `Generic
  | "male" -> Some `Male
  | _ -> None

(** [sex_type ?include_generic f]: a random sex type ([`Generic] only if
    [include_generic]). *)
let sex_type ?(include_generic = false) f : sex_type =
  if include_generic then Fk_helpers.array_element [| `Female; `Generic; `Male |] f
  else Fk_helpers.array_element [| `Female; `Male |] f

let member k v = match Json.member k v with Some Json.Null | None -> None | x -> x
let to_array = function Json.Arr a -> a | v -> [| v |]

(* selectDefinition: [entry] is a {generic?, female?, male?} definition. *)
let select_definition f (sex : [< sex_type ] option) (entry : Json.t) : Json.t array =
  let sex = match sex with Some s -> (s :> sex_type) | None -> sex_type f in
  let generic = member "generic" entry
  and female = member "female" entry
  and male = member "male" entry in
  match sex with
  | `Generic -> (
      match generic with
      | Some g -> to_array g
      | None -> (
          match Fk_helpers.array_element [| female; male |] f with
          | Some v -> to_array v
          | None -> [||]))
  | (`Female | `Male) as s -> (
      let binary = if s = `Female then female else male in
      match binary with
      | Some binary -> (
          let binary = to_array binary in
          match generic with
          | Some generic ->
              let generic = to_array generic in
              Fk_helpers.weighted_array_element
                [|
                  (Js.mul 3.0 (Float.sqrt (float_of_int (Array.length binary))), binary);
                  (Float.sqrt (float_of_int (Array.length generic)), generic);
                |]
                f
          | None -> binary)
      | None -> ( match generic with Some g -> to_array g | None -> [||]))

let weighted_of_json (a : Json.t array) : (float * Json.t) array =
  Array.map
    (fun o ->
      let w = match Json.member "weight" o with Some (Json.Num n) -> n | _ -> nan in
      let v = match Json.member "value" o with Some v -> v | None -> Json.Null in
      (w, v))
    a

let el_json a f = Locale.to_string (Fk_helpers.array_element a f)

let first_name ?sex f =
  let entry = Locale.get f "person" "first_name" in
  el_json (select_definition f sex entry) f

let last_name ?sex f =
  match Locale.find f "person" "last_name_pattern" with
  | Some patterns ->
      let pattern =
        Fk_helpers.weighted_array_element (weighted_of_json (select_definition f sex patterns)) f
      in
      Fake.fake (Locale.to_string pattern) f
  | None ->
      let entry = Locale.get f "person" "last_name" in
      el_json (select_definition f sex entry) f

let middle_name ?sex f =
  let entry = Locale.get f "person" "middle_name" in
  el_json (select_definition f sex entry) f

let prefix ?sex f =
  let entry = Locale.get f "person" "prefix" in
  el_json (select_definition f sex entry) f

let el entry f = Fk_helpers.array_element (Locale.strings f "person" entry) f
let suffix f = el "suffix" f

(** [full_name ?first_name ?last_name ?sex f]: [sex] defaults to a random
    [`Female] / [`Male]. *)
let full_name ?first_name:fn ?last_name:ln ?sex f =
  let sex : sex_type =
    match sex with Some s -> (s :> sex_type) | None -> Fk_helpers.array_element [| `Female; `Male |] f
  in
  let fn = match fn with Some v -> v | None -> first_name ~sex f in
  let ln = match ln with Some v -> v | None -> last_name ~sex f in
  let pattern =
    Fk_helpers.weighted_array_element (weighted_of_json (to_array (Locale.get f "person" "name"))) f
  in
  Fake.mustache
    (Some (Locale.to_string pattern))
    [
      ("person.prefix", `F (fun _ -> prefix ~sex f));
      ("person.firstName", `F (fun _ -> fn));
      ("person.middleName", `F (fun _ -> middle_name ~sex f));
      ("person.lastName", `F (fun _ -> ln));
      ("person.suffix", `F (fun _ -> suffix f));
    ]

let gender f = el "gender" f
let sex f = el "sex" f
let bio f = Fake.fake_json (Locale.get f "person" "bio_pattern") f
let job_title f = Fake.fake_json (Locale.get f "person" "job_title_pattern") f
let job_descriptor f = el "job_descriptor" f
let job_area f = el "job_area" f
let job_type f = el "job_type" f
let zodiac_sign f = el "western_zodiac_sign" f

let registry : (string * Registry.fn) list =
  let sex_fn = sex in
  let open Args in
  let sex_arg a =
    match nth a 0 with Some (Json.Str s) -> sex_type_of_string s | _ -> None
  in
  let s g = fun f _ -> str (g f) in
  [
    ("firstName", fun f a -> str (first_name ?sex:(sex_arg a) f));
    ("lastName", fun f a -> str (last_name ?sex:(sex_arg a) f));
    ("middleName", fun f a -> str (middle_name ?sex:(sex_arg a) f));
    ( "fullName",
      fun f a ->
        let o = opts a in
        str
          (full_name ?first_name:(string o "firstName") ?last_name:(string o "lastName")
             ?sex:(Option.bind (string o "sex") sex_type_of_string)
             f) );
    ("gender", s gender);
    ("sex", s sex_fn);
    ( "sexType",
      fun f a ->
        let o = opts a in
        str (sex_type_to_string (sex_type ?include_generic:(bool o "includeGeneric") f)) );
    ("bio", s bio);
    ("prefix", fun f a -> str (prefix ?sex:(sex_arg a) f));
    ("suffix", s suffix);
    ("jobTitle", s job_title);
    ("jobDescriptor", s job_descriptor);
    ("jobArea", s job_area);
    ("jobType", s job_type);
    ("zodiacSign", s zodiac_sign);
  ]
