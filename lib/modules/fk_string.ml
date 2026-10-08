(* Port of src/modules/string/module.ts, uuid.ts and src/internal/base32.ts. *)

let upper_chars = Array.init 26 (fun i -> String.make 1 (Char.chr (65 + i)))
let lower_chars = Array.init 26 (fun i -> String.make 1 (Char.chr (97 + i)))
let digit_chars = Array.init 10 (fun i -> String.make 1 (Char.chr (48 + i)))

let from_character_array ?(length = `N 1) (characters : string array) f =
  let length = Fk_helpers.range_to_number length f in
  if length <= 0 then ""
  else begin
    if Array.length characters = 0 then
      Core.error "Unable to generate string: No characters to select from.";
    String.concat ""
      (Array.to_list
         (Fk_helpers.multiple ~count:(`N length)
            (fun _ -> Fk_helpers.array_element characters f)
            f))
  end

(** [from_characters ?length chars]: [chars] is split into code points. *)
let from_characters ?length (characters : string) f =
  from_character_array ?length (Array.of_list (Js.code_points characters)) f

let casing_chars ~(casing : Types.casing) =
  match casing with
  | `Upper -> upper_chars
  | `Lower -> lower_chars
  | `Mixed -> Array.append lower_chars upper_chars

let filter_exclude exclude chars =
  Array.of_list
    (List.filter (fun c -> not (List.mem c exclude)) (Array.to_list chars))

let alpha ?(length = `N 1) ?(casing = `Mixed) ?(exclude = []) f =
  let length = Fk_helpers.range_to_number length f in
  if length <= 0 then ""
  else
    from_character_array ~length:(`N length)
      (filter_exclude exclude (casing_chars ~casing))
      f

let alphanumeric ?(length = `N 1) ?(casing = `Mixed) ?(exclude = []) f =
  let length = Fk_helpers.range_to_number length f in
  if length <= 0 then ""
  else
    let chars = Array.append digit_chars (casing_chars ~casing) in
    from_character_array ~length:(`N length) (filter_exclude exclude chars) f

let binary ?(length = `N 1) ?(prefix = "0b") f =
  prefix ^ from_character_array ~length [| "0"; "1" |] f

let octal ?(length = `N 1) ?(prefix = "0o") f =
  prefix
  ^ from_character_array ~length (Array.init 8 (fun i -> string_of_int i)) f

let hex_chars = Array.of_list (Js.code_points "0123456789abcdefABCDEF")

let hexadecimal ?(length = `N 1) ?(casing = `Mixed) ?(prefix = "0x") f =
  let length = Fk_helpers.range_to_number length f in
  if length <= 0 then prefix
  else
    let s = from_character_array ~length:(`N length) hex_chars f in
    let s =
      match casing with
      | `Upper -> String.uppercase_ascii s
      | `Lower -> String.lowercase_ascii s
      | `Mixed -> s
    in
    prefix ^ s

let numeric ?(length = `N 1) ?(allow_leading_zeros = true) ?(exclude = []) f =
  let length = Fk_helpers.range_to_number length f in
  if length <= 0 then ""
  else
    let allowed = filter_exclude exclude digit_chars in
    if
      Array.length allowed = 0
      || Array.length allowed = 1
         && (not allow_leading_zeros)
         && allowed.(0) = "0"
    then
      Core.error
        "Unable to generate numeric string, because all possible digits are \
         excluded.";
    let first =
      if (not allow_leading_zeros) && not (List.mem "0" exclude) then
        Fk_helpers.array_element (filter_exclude [ "0" ] allowed) f
      else ""
    in
    first
    ^ from_character_array ~length:(`N (length - String.length first)) allowed f

let sample ?(length = `N 10) f =
  let length = Fk_helpers.range_to_number length f in
  let b = Buffer.create length in
  while Buffer.length b < length do
    Buffer.add_char b (Char.chr (Fk_number.int ~min:33 ~max:125 f))
  done;
  Buffer.contents b

(* ---------- uuid / ulid ---------- *)

let replace_with s ch gen =
  String.concat ""
    (List.map
       (fun c -> if c = ch then gen () else String.make 1 c)
       (List.of_seq (String.to_seq s)))

let uuid_random_part pattern f =
  let s = replace_with pattern 'x' (fun () -> Fk_number.hex ~min:0 ~max:15 f) in
  replace_with s 'y' (fun () -> Fk_number.hex ~min:8 ~max:11 f)

let uuid_v4 f = uuid_random_part "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx" f

let uuid_v7 f ref_date =
  let ms = int_of_float (Float.max ref_date 0.0) in
  let h = Js.pad_start (Js.int_to_radix ms 16) 12 '0' in
  let h = Js.slice h (-12) in
  let time_part = String.sub h 0 8 ^ "-" ^ String.sub h 8 4 in
  time_part ^ "-" ^ uuid_random_part "7xxx-yxxx-xxxxxxxxxxxx" f

let uuid ?(version = `V4) ?ref_date (f : Core.t) =
  let ref_date = match ref_date with Some d -> d | None -> Core.ref_date f in
  match version with `V7 -> uuid_v7 f ref_date | `V4 -> uuid_v4 f

let crockfords_base32 = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

let date_to_base32 (ms : float) =
  let value = ref (Int64.of_float ms) in
  let b = Bytes.make 10 '0' in
  for i = 9 downto 0 do
    let m = Int64.to_int (Int64.rem !value 32L) in
    let m = if m < 0 then m + 32 else m in
    Bytes.set b i crockfords_base32.[m];
    value := Int64.div (Int64.sub !value (Int64.of_int m)) 32L
  done;
  Bytes.to_string b

let max_ulid_timestamp = 281474976710655.0 (* 2^48 - 1 *)

let ulid ?ref_date (f : Core.t) =
  let date = match ref_date with Some d -> d | None -> Core.ref_date f in
  if date < 0.0 || date > max_ulid_timestamp then
    Core.error
      "Unable to generate ULID: the refDate must be between %s and %s, but was \
       %s."
      (Date_util.to_iso 0.0)
      (Date_util.to_iso max_ulid_timestamp)
      (Date_util.to_iso date);
  date_to_base32 date ^ from_characters ~length:(`N 16) crockfords_base32 f

let nanoid ?(length = `N 21) f =
  let length = Fk_helpers.range_to_number length f in
  if length <= 0 then ""
  else begin
    let generators =
      [|
        (62.0, fun () -> alphanumeric ~length:(`N 1) f);
        (2.0, fun () -> Fk_helpers.array_element [| "_"; "-" |] f);
      |]
    in
    let b = Buffer.create length in
    while Buffer.length b < length do
      let gen = Fk_helpers.weighted_array_element generators f in
      Buffer.add_string b (gen ())
    done;
    Buffer.contents b
  end

let symbol_chars =
  Array.of_list (Js.code_points "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

let symbol ?(length = `N 1) f = from_character_array ~length symbol_chars f

let registry : (string * Registry.fn) list =
  let open Args in
  let len o = range o "length" in
  [
    ( "fromCharacters",
      fun f a ->
        let length =
          match nth a 1 with
          | Some (Json.Num n) -> Some (`N (int_of_float n))
          | Some (Json.Obj _ as r) -> range [ ("l", r) ] "l"
          | _ -> None
        in
        match nth a 0 with
        | Some (Json.Str s) -> str (from_characters ?length s f)
        | Some (Json.Arr c) ->
            str (from_character_array ?length (Array.map Json.to_js_string c) f)
        | _ ->
            Core.error
              "Unable to generate string: No characters to select from." );
    ( "alpha",
      fun f a ->
        let o = opts ~shorthand:"length" a in
        str
          (alpha ?length:(len o) ?casing:(casing o "casing")
             ?exclude:(strings o "exclude") f) );
    ( "alphanumeric",
      fun f a ->
        let o = opts ~shorthand:"length" a in
        str
          (alphanumeric ?length:(len o) ?casing:(casing o "casing")
             ?exclude:(strings o "exclude") f) );
    ( "binary",
      fun f a ->
        let o = opts a in
        str (binary ?length:(len o) ?prefix:(string o "prefix") f) );
    ( "octal",
      fun f a ->
        let o = opts a in
        str (octal ?length:(len o) ?prefix:(string o "prefix") f) );
    ( "hexadecimal",
      fun f a ->
        let o = opts a in
        str
          (hexadecimal ?length:(len o) ?casing:(casing o "casing")
             ?prefix:(string o "prefix") f) );
    ( "numeric",
      fun f a ->
        let o = opts ~shorthand:"length" a in
        str
          (numeric ?length:(len o)
             ?allow_leading_zeros:(bool o "allowLeadingZeros")
             ?exclude:(strings o "exclude") f) );
    ( "sample",
      fun f a ->
        let o = opts ~shorthand:"length" a in
        str (sample ?length:(len o) f) );
    ( "uuid",
      fun f a ->
        let o = opts a in
        let version =
          match int o "version" with Some 7 -> Some `V7 | _ -> None
        in
        str (uuid ?version ?ref_date:(date o "refDate") f) );
    ( "ulid",
      fun f a ->
        let o = opts a in
        str (ulid ?ref_date:(date o "refDate") f) );
    ( "nanoid",
      fun f a ->
        let o = opts ~shorthand:"length" a in
        str (nanoid ?length:(len o) f) );
    ( "symbol",
      fun f a ->
        let o = opts ~shorthand:"length" a in
        str (symbol ?length:(len o) f) );
  ]
