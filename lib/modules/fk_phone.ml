(* Port of src/modules/phone/module.ts. *)

type style = [ `Human | `National | `International | `Mobile ]

let style_to_string : style -> string = function
  | `Human -> "human"
  | `National -> "national"
  | `International -> "international"
  | `Mobile -> "mobile"

let number_with_style_name style f =
  let formats = Locale.get f "phone_number" "format" in
  let definitions =
    match Json.member style formats with
    | None | Some Json.Null ->
        Core.error "No definitions for %s in this locale" style
    | Some d -> Locale.to_strings d
  in
  let format = Fk_helpers.array_element definitions f in
  Fk_helpers.legacy_replace_symbol_with_number format f

(** [number ?style f]: [style] defaults to [`Human]. *)
let number ?(style : style = `Human) f =
  number_with_style_name (style_to_string style) f

let imei f =
  Fk_helpers.replace_credit_card_symbols ~symbol:'#' "##-######-######-L" f

let registry : (string * Registry.fn) list =
  let open Args in
  [
    ( "number",
      fun f a ->
        let o = opts a in
        str
          (number_with_style_name
             (Option.value ~default:"human" (string o "style"))
             f) );
    ("imei", fun f _ -> str (imei f));
  ]
