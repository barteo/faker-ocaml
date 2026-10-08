(* Decoding of JSON call parameters for registry entries, e.g. the
   [{"min":1,"max":5}] in "{{number.int({"min":1,"max":5})}}". *)

type opts = (string * Json.t) list

(** Options object from the first argument. A non-object first argument is
    mapped to [shorthand] (e.g. [number.int(5)] is [{max: 5}]). *)
let opts ?shorthand (args : Json.t list) : opts =
  match (args, shorthand) with
  | Json.Obj kvs :: _, _ -> kvs
  | (Json.Null :: _ | []), _ -> []
  | v :: _, Some key -> [ (key, v) ]
  | _ :: _, None -> []

let get (o : opts) key =
  match List.assoc_opt key o with Some Json.Null | None -> None | v -> v

let float o key = match get o key with Some (Json.Num n) -> Some n | _ -> None
let int o key = Option.map int_of_float (float o key)

(** Decodes a [bigint] option like JS [BigInt(...)]: numbers, strings and
    booleans. *)
let bigint o key =
  match get o key with
  | Some (Json.Num n) -> Some (Bigint.of_float n)
  | Some (Json.Str s) -> Some (Bigint.of_string s)
  | Some (Json.Bool b) -> Some (Bigint.of_bool b)
  | _ -> None

let string o key =
  match get o key with Some (Json.Str s) -> Some s | _ -> None

let bool o key = match get o key with Some (Json.Bool b) -> Some b | _ -> None

let range o key : Types.range option =
  match get o key with
  | Some (Json.Num n) -> Some (`N (int_of_float n))
  | Some (Json.Obj _ as r) -> (
      match (Json.member "min" r, Json.member "max" r) with
      | Some (Json.Num a), Some (Json.Num b) ->
          Some (`Range (int_of_float a, int_of_float b))
      | _ -> None)
  | _ -> None

let casing o key : Types.casing option =
  match string o key with
  | Some "upper" -> Some `Upper
  | Some "lower" -> Some `Lower
  | Some "mixed" -> Some `Mixed
  | _ -> None

let sex o key : Types.sex option =
  match string o key with
  | Some "female" -> Some `Female
  | Some "male" -> Some `Male
  | _ -> None

let strings o key =
  match get o key with
  | Some (Json.Arr a) -> Some (Array.to_list (Array.map Json.to_js_string a))
  | Some (Json.Str s) -> Some (Js.code_points s)
  | _ -> None

(** Dates may be given as ISO strings or epoch milliseconds. *)
let date o key =
  match get o key with
  | Some (Json.Num n) -> Some n
  | Some (Json.Str s) -> Some (Date_util.of_iso s)
  | _ -> None

(* positional helpers *)
let nth (args : Json.t list) i = List.nth_opt args i
let str s = Json.Str s
let num n = Json.Num n
let int_ i = Json.Num (float_of_int i)
let bool_ b = Json.Bool b
let strs a = Json.Arr (Array.map (fun s -> Json.Str s) a)
