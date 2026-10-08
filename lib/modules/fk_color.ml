(* Port of src/modules/color/module.ts.

   Upstream methods return either a string or a number array depending on the
   [format] option. In OCaml this is split by return type:

   - [rgb ?prefix ?casing ?format ?include_alpha f : string] with
     [format : [`Hex | `Css | `Binary]] (default [`Hex]), and
     [rgb_decimal ?include_alpha f : float array] for [format: 'decimal'].
   - [cmyk], [hsl], [hwb], [lab], [lch] and [color_by_css_color_space] return
     the default [float array] ([format: 'decimal']); the [*_string] variants
     take [?format : [`Css | `Binary]] (default [`Css]) and return a string.
   - [human], [space], [css_supported_function] and [css_supported_space]
     return strings. *)

type string_format = [ `Css | `Binary ]
type css_space = [ `SRGB | `Display_p3 | `Rec2020 | `A98_rgb | `Prophoto_rgb ]

type css_function =
  [ `Rgb | `Rgba | `Hsl | `Hsla | `Hwb | `Cmyk | `Lab | `Lch | `Color ]

let css_space_to_string : css_space -> string = function
  | `SRGB -> "sRGB"
  | `Display_p3 -> "display-p3"
  | `Rec2020 -> "rec2020"
  | `A98_rgb -> "a98-rgb"
  | `Prophoto_rgb -> "prophoto-rgb"

let css_space_of_string = function
  | "sRGB" -> Some `SRGB
  | "display-p3" -> Some `Display_p3
  | "rec2020" -> Some `Rec2020
  | "a98-rgb" -> Some `A98_rgb
  | "prophoto-rgb" -> Some `Prophoto_rgb
  | _ -> None

(* Enum declaration order. *)
let css_functions =
  [| "rgb"; "rgba"; "hsl"; "hsla"; "hwb"; "cmyk"; "lab"; "lch"; "color" |]

let css_spaces =
  [| "sRGB"; "display-p3"; "rec2020"; "a98-rgb"; "prophoto-rgb" |]

let format_hex_color hex_color ~prefix ~(casing : Types.casing) =
  let hex_color =
    match casing with
    | `Upper -> String.uppercase_ascii hex_color
    | `Lower -> String.lowercase_ascii hex_color
    | `Mixed -> hex_color
  in
  if prefix <> "" then prefix ^ hex_color else hex_color

let byte_to_binary n = Js.pad_start (Js.int_to_radix n 2) 8 '0'

let to_binary (values : float array) : string =
  String.concat " "
    (Array.to_list
       (Array.map
          (fun value ->
            let is_float = Float.rem value 1.0 <> 0.0 in
            if is_float then
              let bits = Int32.bits_of_float value in
              String.concat ""
                (List.map
                   (fun shift ->
                     byte_to_binary
                       (Int32.to_int (Int32.shift_right_logical bits shift)
                       land 0xff))
                   [ 24; 16; 8; 0 ])
            else
              (* (value >>> 0).toString(2) *)
              let u =
                Int64.to_int (Int64.logand (Int64.of_float value) 0xFFFFFFFFL)
              in
              byte_to_binary u)
          values))

let to_percentage (value : float) : string =
  Js.number_to_string (Float.floor (Js.mul value 100.0 +. 0.5))

let to_css (values : float array) (css_function : css_function)
    (space : css_space) : string =
  let v i = Js.number_to_string values.(i) in
  let p i = to_percentage values.(i) in
  match css_function with
  | `Rgba -> Printf.sprintf "rgba(%s, %s, %s, %s)" (v 0) (v 1) (v 2) (v 3)
  | `Color ->
      Printf.sprintf "color(%s %s %s %s)"
        (css_space_to_string space)
        (v 0) (v 1) (v 2)
  | `Cmyk ->
      Printf.sprintf "cmyk(%s%%, %s%%, %s%%, %s%%)" (p 0) (p 1) (p 2) (p 3)
  | `Hsl -> Printf.sprintf "hsl(%sdeg %s%% %s%%)" (v 0) (p 1) (p 2)
  | `Hsla -> Printf.sprintf "hsl(%sdeg %s%% %s%% / %s)" (v 0) (p 1) (p 2) (v 3)
  | `Hwb -> Printf.sprintf "hwb(%s %s%% %s%%)" (v 0) (p 1) (p 2)
  | `Lab -> Printf.sprintf "lab(%s%% %s %s)" (p 0) (v 1) (v 2)
  | `Lch -> Printf.sprintf "lch(%s%% %s %s)" (p 0) (v 1) (v 2)
  | `Rgb -> Printf.sprintf "rgb(%s, %s, %s)" (v 0) (v 1) (v 2)

let to_string_format values (format : string_format) css_function
    ?(space = `SRGB) () =
  match format with
  | `Css -> to_css values css_function space
  | `Binary -> to_binary values

(* ---------- methods ---------- *)

let human f = Fk_helpers.array_element (Locale.strings f "color" "human") f
let space f = Fk_helpers.array_element (Locale.strings f "color" "space") f
let css_supported_function f = Fk_helpers.array_element css_functions f
let css_supported_space f = Fk_helpers.array_element css_spaces f
let pct f = Fk_number.float ~multiple_of:0.01 f

let rgb_values include_alpha f =
  let a = Array.init 3 (fun _ -> float_of_int (Fk_number.int ~max:255 f)) in
  if include_alpha then Array.append a [| pct f |] else a

(** [rgb ?prefix ?casing ?format ?include_alpha f]: [format] is [`Hex]
    (default), [`Css] or [`Binary]. [prefix] (default ["#"]) and [casing]
    (default [`Lower]) only apply to [`Hex]. *)
let rgb ?(prefix = "#") ?(casing = `Lower) ?(format = `Hex)
    ?(include_alpha = false) f : string =
  match format with
  | `Hex ->
      let color =
        Fk_string.hexadecimal
          ~length:(`N (if include_alpha then 8 else 6))
          ~prefix:"" f
      in
      format_hex_color color ~prefix ~casing
  | (`Css | `Binary) as format ->
      let color = rgb_values include_alpha f in
      to_string_format color format (if include_alpha then `Rgba else `Rgb) ()

(** [rgb] with [format: 'decimal']. *)
let rgb_decimal ?(include_alpha = false) f : float array =
  rgb_values include_alpha f

let cmyk f : float array = Array.init 4 (fun _ -> pct f)
let cmyk_string ?(format = `Css) f = to_string_format (cmyk f) format `Cmyk ()

let hsl_values include_alpha f =
  let h = float_of_int (Fk_number.int ~max:360 f) in
  let rest = Array.init (if include_alpha then 3 else 2) (fun _ -> pct f) in
  Array.append [| h |] rest

let hsl ?(include_alpha = false) f : float array = hsl_values include_alpha f

let hsl_string ?(format = `Css) ?(include_alpha = false) f =
  to_string_format
    (hsl_values include_alpha f)
    format
    (if include_alpha then `Hsla else `Hsl)
    ()

let hwb f : float array =
  let h = float_of_int (Fk_number.int ~max:360 f) in
  Array.append [| h |] (Array.init 2 (fun _ -> pct f))

let hwb_string ?(format = `Css) f = to_string_format (hwb f) format `Hwb ()

let lab f : float array =
  let l = Fk_number.float ~multiple_of:0.000001 f in
  Array.append [| l |]
    (Array.init 2 (fun _ ->
         Fk_number.float ~min:(-100.0) ~max:100.0 ~multiple_of:0.0001 f))

let lab_string ?(format = `Css) f = to_string_format (lab f) format `Lab ()

let lch f : float array =
  let l = Fk_number.float ~multiple_of:0.000001 f in
  let c = Fk_number.float ~max:230.0 ~multiple_of:0.1 f in
  let h = Fk_number.float ~max:360.0 ~multiple_of:0.1 f in
  [| l; c; h |]

let lch_string ?(format = `Css) f = to_string_format (lch f) format `Lch ()

(** [space] only matters for the string variant (default [`SRGB]). *)
let color_by_css_color_space f : float array =
  Array.init 3 (fun _ -> Fk_number.float ~multiple_of:0.0001 f)

let color_by_css_color_space_string ?(format = `Css) ?(space = `SRGB) f =
  to_string_format (color_by_css_color_space f) format `Color ~space ()

(* ---------- registry ---------- *)

let registry : (string * Registry.fn) list =
  let open Args in
  let nums a = Json.Arr (Array.map (fun x -> Json.Num x) a) in
  let fmt o = string o "format" in
  let sfmt o = match fmt o with Some "binary" -> `Binary | _ -> `Css in
  let by_format o ~decimal ~str_ =
    match fmt o with
    | Some ("css" | "binary") -> str (str_ (sfmt o))
    | _ -> nums (decimal ())
  in
  [
    ("human", fun f _ -> str (human f));
    ("space", fun f _ -> str (space f));
    ("cssSupportedFunction", fun f _ -> str (css_supported_function f));
    ("cssSupportedSpace", fun f _ -> str (css_supported_space f));
    ( "rgb",
      fun f a ->
        let o = opts a in
        let include_alpha = bool o "includeAlpha" in
        match fmt o with
        | Some "decimal" -> nums (rgb_decimal ?include_alpha f)
        | v ->
            let format =
              match v with
              | Some "css" -> `Css
              | Some "binary" -> `Binary
              | _ -> `Hex
            in
            str
              (rgb ?prefix:(string o "prefix") ?casing:(casing o "casing")
                 ~format ?include_alpha f) );
    ( "cmyk",
      fun f a ->
        let o = opts a in
        by_format o
          ~decimal:(fun () -> cmyk f)
          ~str_:(fun format -> cmyk_string ~format f) );
    ( "hsl",
      fun f a ->
        let o = opts a in
        let include_alpha = bool o "includeAlpha" in
        by_format o
          ~decimal:(fun () -> hsl ?include_alpha f)
          ~str_:(fun format -> hsl_string ~format ?include_alpha f) );
    ( "hwb",
      fun f a ->
        let o = opts a in
        by_format o
          ~decimal:(fun () -> hwb f)
          ~str_:(fun format -> hwb_string ~format f) );
    ( "lab",
      fun f a ->
        let o = opts a in
        by_format o
          ~decimal:(fun () -> lab f)
          ~str_:(fun format -> lab_string ~format f) );
    ( "lch",
      fun f a ->
        let o = opts a in
        by_format o
          ~decimal:(fun () -> lch f)
          ~str_:(fun format -> lch_string ~format f) );
    ( "colorByCSSColorSpace",
      fun f a ->
        let o = opts a in
        let space = Option.bind (string o "space") css_space_of_string in
        by_format o
          ~decimal:(fun () -> color_by_css_color_space f)
          ~str_:(fun format -> color_by_css_color_space_string ~format ?space f)
    );
  ]
