(* Port of src/modules/airline/module.ts. *)

type aircraft_type = [ `Narrowbody | `Regional | `Widebody ]
type airline = { name : string; iata_code : string }
type airplane = { name : string; iata_type_code : string }
type airport = { name : string; iata_code : string }

let aircraft_type_to_string : aircraft_type -> string = function
  | `Narrowbody -> "narrowbody"
  | `Regional -> "regional"
  | `Widebody -> "widebody"

let aircraft_type_of_string = function
  | "narrowbody" -> Some `Narrowbody
  | "regional" -> Some `Regional
  | "widebody" -> Some `Widebody
  | _ -> None

let numerics = [ "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ]
let visually_similar_characters = [ "0"; "O"; "1"; "I"; "L" ]

let aircraft_type_max_rows : aircraft_type -> int = function
  | `Regional -> 20
  | `Narrowbody -> 35
  | `Widebody -> 60

let aircraft_type_seats : aircraft_type -> string array = function
  | `Regional -> [| "A"; "B"; "C"; "D" |]
  | `Narrowbody -> [| "A"; "B"; "C"; "D"; "E"; "F" |]
  | `Widebody -> [| "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "J"; "K" |]

(* Picks one object of an airline.<entry> data array. *)
let pick_obj entry f =
  match Locale.get f "airline" entry with
  | Json.Arr a -> Fk_helpers.array_element a f
  | v -> v

let field o k = match Json.member k o with Some v -> Locale.to_string v | None -> ""

let airport f : airport =
  let o = pick_obj "airport" f in
  { name = field o "name"; iata_code = field o "iataCode" }

let airline f : airline =
  let o = pick_obj "airline" f in
  { name = field o "name"; iata_code = field o "iataCode" }

let airplane f : airplane =
  let o = pick_obj "airplane" f in
  { name = field o "name"; iata_type_code = field o "iataTypeCode" }

let record_locator ?(allow_numerics = false) ?(allow_visually_similar_characters = false) f =
  let excluded = (if allow_numerics then [] else numerics)
    @ if allow_visually_similar_characters then [] else visually_similar_characters
  in
  Fk_string.alphanumeric ~length:(`N 6) ~casing:`Upper ~exclude:excluded f

let seat ?(aircraft_type : aircraft_type = `Narrowbody) f =
  let max_row = aircraft_type_max_rows aircraft_type in
  let allowed_seats = aircraft_type_seats aircraft_type in
  let row = Fk_number.int ~min:1 ~max:max_row f in
  let seat = Fk_helpers.array_element allowed_seats f in
  string_of_int row ^ seat

(* helpers.enumValue(Aircraft): keys in declaration order. *)
let aircraft_type f : aircraft_type =
  Fk_helpers.array_element [| `Narrowbody; `Regional; `Widebody |] f

let flight_number ?(length = `Range (1, 4)) ?(add_leading_zeros = false) f =
  let flight_number = Fk_string.numeric ~length ~allow_leading_zeros:false f in
  if add_leading_zeros then Js.pad_start flight_number 4 '0' else flight_number

(* JSON views (JS key order). *)
let airline_to_json (a : airline) = Json.Obj [ ("name", Json.Str a.name); ("iataCode", Json.Str a.iata_code) ]
let airport_to_json (a : airport) = Json.Obj [ ("name", Json.Str a.name); ("iataCode", Json.Str a.iata_code) ]

let airplane_to_json (a : airplane) =
  Json.Obj [ ("name", Json.Str a.name); ("iataTypeCode", Json.Str a.iata_type_code) ]

let registry : (string * Registry.fn) list =
  let open Args in
  [
    ("airport", fun f _ -> airport_to_json (airport f));
    ("airline", fun f _ -> airline_to_json (airline f));
    ("airplane", fun f _ -> airplane_to_json (airplane f));
    ( "recordLocator",
      fun f a ->
        let o = opts a in
        str
          (record_locator ?allow_numerics:(bool o "allowNumerics")
             ?allow_visually_similar_characters:(bool o "allowVisuallySimilarCharacters") f) );
    ( "seat",
      fun f a ->
        let o = opts a in
        let aircraft_type = Option.bind (string o "aircraftType") aircraft_type_of_string in
        str (seat ?aircraft_type f) );
    ("aircraftType", fun f _ -> str (aircraft_type_to_string (aircraft_type f)));
    ( "flightNumber",
      fun f a ->
        let o = opts a in
        str (flight_number ?length:(range o "length") ?add_leading_zeros:(bool o "addLeadingZeros") f) );
  ]
