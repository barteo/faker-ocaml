(* Port of src/modules/location/module.ts (SimpleLocationModule and LocationModule). *)

type language = { name : string; alpha2 : string; alpha3 : string }

let language_to_json (l : language) : Json.t =
  Json.Obj [ ("name", Json.Str l.name); ("alpha2", Json.Str l.alpha2); ("alpha3", Json.Str l.alpha3) ]

(* ---------- SimpleLocationModule ---------- *)

(** A random latitude in [min, max] (default [-90, 90]) with [precision] (default 4) fraction digits. *)
let latitude ?(max = 90.0) ?(min = -90.0) ?(precision = 4) f =
  Fk_number.float ~min ~max ~fraction_digits:precision f

(** A random longitude in [min, max] (default [-180, 180]) with [precision] (default 4) fraction digits. *)
let longitude ?(max = 180.0) ?(min = -180.0) ?(precision = 4) f =
  Fk_number.float ~max ~min ~fraction_digits:precision f

let sign x = if x > 0.0 then 1.0 else if x < 0.0 then -1.0 else x

(** A random [(latitude, longitude)] within [radius] (default 10, miles unless [is_metric])
    of [origin]; a random coordinate when [origin] is omitted. *)
let nearby_gps_coordinate ?origin ?(radius = 10.0) ?(is_metric = false) f : float * float =
  match origin with
  | None ->
      let lat = latitude f in
      let lon = longitude f in
      (lat, lon)
  | Some (origin_lat, origin_lon) ->
      let angle_radians = Fk_number.float ~max:(2.0 *. Float.pi) ~fraction_digits:5 f in
      let radius_metric = if is_metric then radius else radius *. 1.60934 in
      let error_correction = 0.995 in
      let distance_in_km =
        Fk_number.float ~max:radius_metric ~fraction_digits:3 f *. error_correction
      in
      let km_per_degree = 40_000.0 /. 360.0 in
      let distance_in_degree = distance_in_km /. km_per_degree in
      let c0 = origin_lat +. Js.mul (Fk_location_math.sin angle_radians) distance_in_degree in
      let c1 = origin_lon +. Js.mul (Fk_location_math.cos angle_radians) distance_in_degree in
      let c0 = Float.rem c0 180.0 in
      let c0, c1 =
        if Float.abs c0 > 90.0 then (Js.mul (sign c0) 180.0 -. c0, c1 +. 180.0) else (c0, c1)
      in
      let c1 = Float.rem (Float.rem c1 360.0 +. 540.0) 360.0 -. 180.0 in
      (c0, c1)

(* ---------- LocationModule ---------- *)

let loc f entry = Locale.get f "location" entry
let pick f entry = Fk_helpers.array_element (Locale.strings f "location" entry) f

(* str.replaceAll(/#+/g, (m) => faker.string.numeric({ length: m.length, allowLeadingZeros: false })) *)
let replace_hash_runs s f =
  let len = String.length s in
  let b = Buffer.create len in
  let rec go i =
    if i < len then
      if s.[i] = '#' then begin
        let j = ref i in
        while !j < len && s.[!j] = '#' do incr j done;
        Buffer.add_string b
          (Fk_string.numeric ~length:(`N (!j - i)) ~allow_leading_zeros:false f);
        go !j
      end
      else (Buffer.add_char b s.[i]; go (i + 1))
  in
  go 0;
  Buffer.contents b

(** A random zip code, from [state] (via [postcode_by_state]) or from [format]
    (default: the locale's [postcode] formats). *)
let zip_code ?state ?format f =
  match state with
  | Some state -> (
      let by_state = loc f "postcode_by_state" in
      match Json.member state by_state with
      | None | Some Json.Null -> Core.error "No zip code definition found for state \"%s\"" state
      | Some pattern -> Fake.fake_json pattern f)
  | None ->
      let formats =
        match format with Some fmt -> [| fmt |] | None -> Locale.to_strings (loc f "postcode")
      in
      let format = Fk_helpers.array_element formats f in
      Fk_helpers.replace_symbols format f

(** A random city name. *)
let city f = Fake.fake_json (loc f "city_pattern") f

(** A random building number. *)
let building_number f = replace_hash_runs (pick f "building_number") f

(** A random street name. *)
let street f = Fake.fake_json (loc f "street_pattern") f

(** A random street address; with [use_full_address] it includes a secondary address. *)
let street_address ?(use_full_address = false) f =
  let formats = loc f "street_address" in
  let key = if use_full_address then "full" else "normal" in
  let format = match Json.member key formats with Some v -> v | None -> Json.Null in
  Fake.fake_json format f

(** A random postal address. *)
let postal_address f = Fake.fake_json (loc f "postal_address") f

(** A random secondary address (e.g. "Apt. 861"). *)
let secondary_address f =
  replace_hash_runs (Fake.fake_json (loc f "secondary_address") f) f

(** A random county name. *)
let county f = pick f "county"

(** A random country name. *)
let country f = pick f "country"

(** A random continent name. *)
let continent f = pick f "continent"

type country_code_variant = [ `Alpha_2 | `Alpha_3 | `Numeric ]

let country_code_entry key f =
  let codes = match loc f "country_code" with Json.Arr a -> a | v -> [| v |] in
  let entry = Fk_helpers.array_element codes f in
  match key with None -> Json.Null | Some k -> Option.value ~default:Json.Null (Json.member k entry)

(** A random ISO 3166-1 country code (default [`Alpha_2]). *)
let country_code ?(variant : country_code_variant = `Alpha_2) f =
  let key = match variant with `Numeric -> "numeric" | `Alpha_3 -> "alpha3" | `Alpha_2 -> "alpha2" in
  Locale.to_string (country_code_entry (Some key) f)

(** A random state name, or its abbreviation. *)
let state ?(abbreviated = false) f = pick f (if abbreviated then "state_abbr" else "state")

let direction_data f key =
  match Json.member key (loc f "direction") with
  | Some v -> Locale.to_strings v
  | None -> [||]

(** A random cardinal or ordinal direction. *)
let direction ?(abbreviated = false) f =
  let data =
    if abbreviated then Array.append (direction_data f "cardinal_abbr") (direction_data f "ordinal_abbr")
    else Array.append (direction_data f "cardinal") (direction_data f "ordinal")
  in
  Fk_helpers.array_element data f

(** A random cardinal direction. *)
let cardinal_direction ?(abbreviated = false) f =
  Fk_helpers.array_element (direction_data f (if abbreviated then "cardinal_abbr" else "cardinal")) f

(** A random ordinal direction. *)
let ordinal_direction ?(abbreviated = false) f =
  Fk_helpers.array_element (direction_data f (if abbreviated then "ordinal_abbr" else "ordinal")) f

(** A random IANA time zone name relevant to the locale. *)
let time_zone f = pick f "time_zone"

let language_json f =
  let langs = match loc f "language" with Json.Arr a -> a | v -> [| v |] in
  Fk_helpers.array_element langs f

(** A random spoken language. *)
let language f : language =
  let l = language_json f in
  let get k = match Json.member k l with Some v -> Locale.to_string v | None -> "" in
  { name = get "name"; alpha2 = get "alpha2"; alpha3 = get "alpha3" }

(* ---------- registry ---------- *)

let registry : (string * Registry.fn) list =
  let open Args in
  let abbr a = bool (opts a) "abbreviated" in
  [
    ( "latitude",
      fun f a ->
        let o = opts a in
        num (latitude ?max:(float o "max") ?min:(float o "min") ?precision:(int o "precision") f) );
    ( "longitude",
      fun f a ->
        let o = opts a in
        num (longitude ?max:(float o "max") ?min:(float o "min") ?precision:(int o "precision") f) );
    ( "nearbyGPSCoordinate",
      fun f a ->
        let o = opts a in
        let origin =
          match get o "origin" with
          | Some (Json.Arr [| lat; lon |]) ->
              let n = function Json.Num n -> n | _ -> Float.nan in
              Some (n lat, n lon)
          | _ -> None
        in
        let lat, lon =
          nearby_gps_coordinate ?origin ?radius:(float o "radius")
            ?is_metric:(bool o "isMetric") f
        in
        Json.Arr [| num lat; num lon |] );
    ( "zipCode",
      fun f a ->
        let o = opts ~shorthand:"format" a in
        str (zip_code ?state:(string o "state") ?format:(string o "format") f) );
    ("city", fun f _ -> str (city f));
    ("buildingNumber", fun f _ -> str (building_number f));
    ("street", fun f _ -> str (street f));
    ( "streetAddress",
      fun f a ->
        let o = opts ~shorthand:"useFullAddress" a in
        let use_full_address =
          match get o "useFullAddress" with
          | None | Some (Json.Bool false) | Some (Json.Num 0.0) | Some (Json.Str "") -> false
          | Some _ -> true
        in
        str (street_address ~use_full_address f) );
    ("postalAddress", fun f _ -> str (postal_address f));
    ("secondaryAddress", fun f _ -> str (secondary_address f));
    ("county", fun f _ -> str (county f));
    ("country", fun f _ -> str (country f));
    ("continent", fun f _ -> str (continent f));
    ( "countryCode",
      fun f a ->
        let o = opts ~shorthand:"variant" a in
        let key =
          match string o "variant" with
          | None | Some "alpha-2" -> Some "alpha2"
          | Some "alpha-3" -> Some "alpha3"
          | Some "numeric" -> Some "numeric"
          | Some _ -> None
        in
        country_code_entry key f );
    ("state", fun f a -> str (state ?abbreviated:(abbr a) f));
    ("direction", fun f a -> str (direction ?abbreviated:(abbr a) f));
    ("cardinalDirection", fun f a -> str (cardinal_direction ?abbreviated:(abbr a) f));
    ("ordinalDirection", fun f a -> str (ordinal_direction ?abbreviated:(abbr a) f));
    ("timeZone", fun f _ -> str (time_zone f));
    ("language", fun f _ -> language_json f);
  ]
