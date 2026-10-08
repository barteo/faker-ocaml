(* Port of src/modules/commerce/module.ts and upc-check-digit.ts. *)

(* Source for official prefixes: https://www.isbn-international.org/range_file_generation *)
let isbn_length_rules : (string * (int * int) array) list =
  [
    ( "0",
      [| (1999999, 2); (2279999, 3); (2289999, 4); (3689999, 3); (3699999, 4); (6389999, 3);
         (6397999, 4); (6399999, 7); (6449999, 3); (6459999, 7); (6479999, 3); (6489999, 7);
         (6549999, 3); (6559999, 4); (6999999, 3); (8499999, 4); (8999999, 5); (9499999, 6);
         (9999999, 7) |] );
    ( "1",
      [| (99999, 3); (299999, 2); (349999, 3); (399999, 4); (499999, 3); (699999, 2);
         (999999, 4); (3979999, 3); (5499999, 4); (6499999, 5); (6799999, 4); (6859999, 5);
         (7139999, 4); (7169999, 3); (7319999, 4); (7399999, 7); (7749999, 5); (7753999, 7);
         (7763999, 5); (7764999, 7); (7769999, 5); (7782999, 7); (7899999, 5); (7999999, 4);
         (8004999, 5); (8049999, 5); (8379999, 5); (8384999, 7); (8671999, 5); (8675999, 4);
         (8697999, 5); (9159999, 6); (9165059, 7); (9168699, 6); (9169079, 7); (9195999, 6);
         (9196549, 7); (9729999, 6); (9877999, 4); (9911499, 6); (9911999, 7); (9989899, 6);
         (9999999, 7) |] );
  ]

let all_digits s = String.for_all (fun c -> c >= '0' && c <= '9') s

(** [calculate_upc_check_digit digits] for exactly 11 digits. *)
let calculate_upc_check_digit (digits : string) : int =
  if not (String.length digits = 11 && all_digits digits) then
    Core.error "calculateUPCCheckDigit expects exactly 11 numeric digits";
  let sum = ref 0 in
  String.iteri
    (fun idx c ->
      let n = Char.code c - 48 in
      sum := !sum + (n * if idx mod 2 = 0 then 3 else 1))
    digits;
  (10 - (!sum mod 10)) mod 10

let product_name_data f key =
  match Locale.path (Locale.get f "commerce" "product_name") [ key ] with
  | Some v -> v
  | None -> Locale.missing ("commerce.product_name." ^ key)

let department f = Fk_helpers.array_element (Locale.strings f "commerce" "department") f
let product_name f = Fake.fake_json (product_name_data f "pattern") f

let price ?(min = 1.0) ?(max = 1000.0) ?(dec = 2) ?(symbol = "") f =
  if min < 0.0 || max < 0.0 then symbol ^ "0"
  else if min = max then symbol ^ Js.to_fixed min dec
  else
    let generated = Fk_number.float ~min ~max ~fraction_digits:dec f in
    if dec = 0 then symbol ^ Js.to_fixed generated dec
    else
      let old_last_digit = Float.rem (Js.mul generated (Float.pow 10.0 (float_of_int dec))) 10.0 in
      let r = float_of_int (Fk_number.int ~min:0 ~max:9 f) in
      let new_last_digit =
        Fk_helpers.weighted_array_element [| (5.0, 9.0); (3.0, 5.0); (1.0, 0.0); (1.0, r) |] f
      in
      let fraction = Float.pow (1.0 /. 10.0) (float_of_int dec) in
      let old_last_digit_value = Js.mul old_last_digit fraction in
      let new_last_digit_value = Js.mul new_last_digit fraction in
      let combined = generated -. old_last_digit_value +. new_last_digit_value in
      if min <= combined && combined <= max then symbol ^ Js.to_fixed combined dec
      else symbol ^ Js.to_fixed generated dec

let product_adjective f =
  Fk_helpers.array_element (Locale.to_strings (product_name_data f "adjective")) f

let product_material f =
  Fk_helpers.array_element (Locale.to_strings (product_name_data f "material")) f

let product f = Fk_helpers.array_element (Locale.to_strings (product_name_data f "product")) f
let product_description f = Fake.fake_json (Locale.get f "commerce" "product_description") f

(** [isbn ?variant ?separator f]: [variant] is [`V10] or [`V13] (default). *)
let isbn ?(variant = `V13) ?(separator = "-") f =
  let prefix = "978" in
  let group, group_rules = Fk_helpers.object_entry isbn_length_rules f in
  let element = Fk_string.numeric ~length:(`N 8) f in
  let element_value = Option.get (Js.parse_int (Js.slice ~end_:(-1) element 0)) in
  let registrant_length =
    match Array.find_opt (fun (range_max, _) -> element_value <= range_max) group_rules with
    | Some (_, l) -> l
    | None -> Core.error "Unable to find a registrant length for the group %s" group
  in
  let registrant = Js.slice ~end_:registrant_length element 0 in
  let publication = Js.slice element registrant_length in
  let data = [ prefix; group; registrant; publication ] in
  let data = if variant = `V10 then List.tl data else data in
  let v = match variant with `V10 -> 10 | `V13 -> 13 in
  let isbn = String.concat "" data in
  let checksum = ref 0 in
  for i = 0 to v - 2 do
    let weight = if v = 10 then i + 1 else if i mod 2 = 1 then 3 else 1 in
    checksum := !checksum + (weight * (Char.code isbn.[i] - 48))
  done;
  let checksum = if v = 10 then !checksum mod 11 else (10 - (!checksum mod 10)) mod 10 in
  String.concat separator
    (data @ [ (if checksum = 10 then "X" else string_of_int checksum) ])

let upc ?(prefix = "") f =
  if prefix <> "" && not (all_digits prefix) then
    Core.error "Prefix must contain only numeric digits";
  if String.length prefix > 11 then Core.error "Prefix must be at most 11 numeric digits";
  let remaining = 11 - String.length prefix in
  let rand = Fk_string.numeric ~length:(`N remaining) ~allow_leading_zeros:true f in
  let body = prefix ^ rand in
  let check = calculate_upc_check_digit body in
  body ^ string_of_int check

let registry : (string * Registry.fn) list =
  let open Args in
  [
    ("department", fun f _ -> str (department f));
    ("productName", fun f _ -> str (product_name f));
    ( "price",
      fun f a ->
        let o = opts a in
        str
          (price ?min:(float o "min") ?max:(float o "max") ?dec:(int o "dec")
             ?symbol:(string o "symbol") f) );
    ("productAdjective", fun f _ -> str (product_adjective f));
    ("productMaterial", fun f _ -> str (product_material f));
    ("product", fun f _ -> str (product f));
    ("productDescription", fun f _ -> str (product_description f));
    ( "isbn",
      fun f a ->
        let o = opts ~shorthand:"variant" a in
        let variant = match int o "variant" with Some 10 -> Some `V10 | Some _ -> Some `V13 | None -> None in
        str (isbn ?variant ?separator:(string o "separator") f) );
    ("upc", fun f a -> let o = opts a in str (upc ?prefix:(string o "prefix") f));
  ]
