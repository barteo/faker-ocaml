(* Port of src/modules/science/module.ts. *)

type chemical_element = { symbol : string; name : string; atomic_number : int }
type unit_ = { name : string; symbol : string }

let pick_obj entry f =
  match Locale.get f "science" entry with Json.Arr a -> Fk_helpers.array_element a f | v -> v

let field o k = match Json.member k o with Some v -> Locale.to_string v | None -> ""

let chemical_element f : chemical_element =
  let o = pick_obj "chemical_element" f in
  {
    symbol = field o "symbol";
    name = field o "name";
    atomic_number = (match Json.member "atomicNumber" o with Some (Json.Num n) -> int_of_float n | _ -> 0);
  }

let unit f : unit_ =
  let o = pick_obj "unit" f in
  { name = field o "name"; symbol = field o "symbol" }

(* JSON views (JS key order). *)
let chemical_element_to_json (e : chemical_element) =
  Json.Obj
    [ ("symbol", Json.Str e.symbol); ("name", Json.Str e.name); ("atomicNumber", Json.int e.atomic_number) ]

let unit_to_json (u : unit_) = Json.Obj [ ("name", Json.Str u.name); ("symbol", Json.Str u.symbol) ]

let registry : (string * Registry.fn) list =
  [
    ("chemicalElement", fun f _ -> chemical_element_to_json (chemical_element f));
    ("unit", fun f _ -> unit_to_json (unit f));
  ]
