(* Port of src/modules/vehicle/module.ts. *)

(* NHTSA 49 CFR 565.15(c), Tables III and IV. *)
let vin_weights = [| 8; 7; 6; 5; 4; 3; 2; 10; 0; 9; 8; 7; 6; 5; 4; 3; 2 |]

let vin_transliteration = function
  | 'A' | 'J' -> Some 1
  | 'B' | 'K' | 'S' -> Some 2
  | 'C' | 'L' | 'T' -> Some 3
  | 'D' | 'M' | 'U' -> Some 4
  | 'E' | 'N' | 'V' -> Some 5
  | 'F' | 'W' -> Some 6
  | 'G' | 'P' | 'X' -> Some 7
  | 'H' | 'Y' -> Some 8
  | 'R' | 'Z' -> Some 9
  | _ -> None

(** [vin_check_digit vin]: the check digit of a 17-character (ASCII) VIN. *)
let vin_check_digit (vin : string) : string =
  let checksum = ref 0 in
  String.iteri
    (fun index c ->
      let value =
        match vin_transliteration c with
        | Some v -> v
        | None -> ( match c with '0' .. '9' -> Char.code c - 48 | _ -> 0)
      in
      checksum := !checksum + (value * vin_weights.(index)))
    vin;
  let r = !checksum mod 11 in
  if r = 10 then "X" else string_of_int r

let el entry f = Fk_helpers.array_element (Locale.strings f "vehicle" entry) f
let manufacturer f = el "manufacturer" f
let model f = el "model" f

let vehicle f =
  let manufacturer = manufacturer f in
  let model = model f in
  manufacturer ^ " " ^ model

let type_ f = el "type" f
let fuel f = el "fuel" f
let bicycle f = el "bicycle_type" f

let vin f =
  let exclude = [ "o"; "i"; "q"; "O"; "I"; "Q" ] in
  let a = Fk_string.alphanumeric ~length:(`N 10) ~casing:`Upper ~exclude f in
  let b = Fk_string.alpha ~length:(`N 1) ~casing:`Upper ~exclude f in
  let c = Fk_string.alphanumeric ~length:(`N 1) ~casing:`Upper ~exclude f in
  let d = Fk_string.numeric ~length:(`N 5) ~allow_leading_zeros:true f in
  let vin = a ^ b ^ c ^ d in
  String.sub vin 0 8 ^ vin_check_digit vin ^ String.sub vin 9 (String.length vin - 9)

let color f = Fk_color.human f

let vrm f =
  let a = Fk_string.alpha ~length:(`N 2) ~casing:`Upper f in
  let b = Fk_string.numeric ~length:(`N 2) ~allow_leading_zeros:true f in
  let c = Fk_string.alpha ~length:(`N 3) ~casing:`Upper f in
  a ^ b ^ c

let registry : (string * Registry.fn) list =
  let open Args in
  [
    ("vehicle", fun f _ -> str (vehicle f));
    ("manufacturer", fun f _ -> str (manufacturer f));
    ("model", fun f _ -> str (model f));
    ("type", fun f _ -> str (type_ f));
    ("fuel", fun f _ -> str (fuel f));
    ("vin", fun f _ -> str (vin f));
    ("color", fun f _ -> str (color f));
    ("vrm", fun f _ -> str (vrm f));
    ("bicycle", fun f _ -> str (bicycle f));
  ]
