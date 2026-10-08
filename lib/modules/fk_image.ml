(* Port of src/modules/image/module.ts (and the toBase64 helper of
   src/internal/base64.ts). *)

type data_uri_type = [ `Svg_uri | `Svg_base64 ]

let data_uri_type_to_string : data_uri_type -> string = function
  | `Svg_uri -> "svg-uri"
  | `Svg_base64 -> "svg-base64"

let data_uri_type_of_string = function
  | "svg-uri" -> Some `Svg_uri
  | "svg-base64" -> Some `Svg_base64
  | _ -> None

(* ---------- encoding helpers ---------- *)

let base64_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

(** [to_base64 s]: standard (padded) Base64 of the UTF-8 bytes of [s], like
    [Buffer.from(s).toString('base64')]. *)
let to_base64 (s : string) : string =
  let n = String.length s in
  let b = Buffer.create (((n + 2) / 3) * 4) in
  let byte i = Char.code (String.unsafe_get s i) in
  let put k = Buffer.add_char b base64_alphabet.[k land 63] in
  let i = ref 0 in
  while !i + 2 < n do
    let x = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
    put (x lsr 18);
    put (x lsr 12);
    put (x lsr 6);
    put x;
    i := !i + 3
  done;
  (match n - !i with
  | 1 ->
      let x = byte !i lsl 16 in
      put (x lsr 18);
      put (x lsr 12);
      Buffer.add_string b "=="
  | 2 ->
      let x = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) in
      put (x lsr 18);
      put (x lsr 12);
      put (x lsr 6);
      Buffer.add_char b '='
  | _ -> ());
  Buffer.contents b

(** [encode_uri_component s]: JS [encodeURIComponent] on the UTF-8 bytes of
    [s] (everything but [A-Za-z0-9-_.!~*'()] is percent-encoded, upper-case hex). *)
let encode_uri_component (s : string) : string =
  let b = Buffer.create (String.length s * 3) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')'
        ->
          Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let dim f = Fk_number.int ~min:1 ~max:3999 f

(* ---------- methods ---------- *)

let avatar_git_hub f =
  "https://avatars.githubusercontent.com/u/" ^ string_of_int (Fk_number.int ~max:100000000 f)

(** [person_portrait ?sex ?size f]: [size] is one of 512 (default), 256, 128, 64, 32. *)
let person_portrait ?(sex : [< Fk_person.sex_type ] option) ?(size = 512) f =
  let sex : Fk_person.sex_type =
    match sex with Some s -> (s :> Fk_person.sex_type) | None -> Fk_person.sex_type f
  in
  let sex = if sex = `Generic then Fk_person.sex_type f else sex in
  let base_url = "https://cdn.jsdelivr.net/gh/faker-js/assets-person-portrait" in
  let n = Fk_number.int ~min:0 ~max:99 f in
  Printf.sprintf "%s/%s/%d/%d.jpg" base_url (Fk_person.sex_type_to_string sex) size n

let avatar f =
  let avatar_method =
    Fk_helpers.array_element [| (fun f -> person_portrait f); avatar_git_hub |] f
  in
  avatar_method f

(** [url_picsum_photos ?width ?height ?grayscale ?blur f]: [blur] 0..10. *)
let url_picsum_photos ?width ?height ?grayscale ?blur f =
  let width = match width with Some w -> w | None -> dim f in
  let height = match height with Some h -> h | None -> dim f in
  let grayscale = match grayscale with Some g -> g | None -> Fk_datatype.boolean f in
  let blur = match blur with Some b -> b | None -> Fk_number.int ~max:10 f in
  let seed = Fk_string.alphanumeric ~length:(`Range (5, 10)) f in
  let url = Printf.sprintf "https://picsum.photos/seed/%s/%d/%d" seed width height in
  let has_valid_blur = blur >= 1 && blur <= 10 in
  if grayscale || has_valid_blur then
    url ^ "?"
    ^ (if grayscale then "grayscale" else "")
    ^ (if grayscale && has_valid_blur then "&" else "")
    ^ if has_valid_blur then "blur=" ^ string_of_int blur else ""
  else url

let url ?width ?height f =
  let width = match width with Some w -> w | None -> dim f in
  let height = match height with Some h -> h | None -> dim f in
  let url_method =
    Fk_helpers.array_element
      [| (fun ~width ~height -> url_picsum_photos ~width ~height ~grayscale:false ~blur:0 f) |]
      f
  in
  url_method ~width ~height

(** Deprecated upstream (since 10.1.0) in favour of [url]. *)
let url_lorem_flickr ?width ?height ?category f =
  let width = match width with Some w -> w | None -> dim f in
  let height = match height with Some h -> h | None -> dim f in
  let category = match category with None -> "" | Some c -> "/" ^ c in
  let lock = Fk_number.int f in
  Printf.sprintf "https://loremflickr.com/%d/%d%s?lock=%d" width height category lock

(** [data_uri ?width ?height ?color ?type_ f]. *)
let data_uri ?width ?height ?color ?(type_ : data_uri_type option) f =
  let width = match width with Some w -> w | None -> dim f in
  let height = match height with Some h -> h | None -> dim f in
  let color = match color with Some c -> c | None -> Fk_color.rgb f in
  let type_ =
    match type_ with Some t -> t | None -> Fk_helpers.array_element [| `Svg_uri; `Svg_base64 |] f
  in
  let half x = Js.number_to_string (float_of_int x /. 2.0) in
  let svg_string =
    Printf.sprintf
      "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" baseProfile=\"full\" \
       width=\"%d\" height=\"%d\"><rect width=\"100%%\" height=\"100%%\" fill=\"%s\"/><text \
       x=\"%s\" y=\"%s\" font-size=\"20\" alignment-baseline=\"middle\" text-anchor=\"middle\" \
       fill=\"white\">%dx%d</text></svg>"
      width height color (half width) (half height) width height
  in
  match type_ with
  | `Svg_uri -> "data:image/svg+xml;charset=UTF-8," ^ encode_uri_component svg_string
  | `Svg_base64 -> "data:image/svg+xml;base64," ^ to_base64 svg_string

(* ---------- registry ---------- *)

let registry : (string * Registry.fn) list =
  let open Args in
  [
    ("avatar", fun f _ -> str (avatar f));
    ("avatarGitHub", fun f _ -> str (avatar_git_hub f));
    ( "personPortrait",
      fun f a ->
        let o = opts a in
        let sex = Option.bind (string o "sex") Fk_person.sex_type_of_string in
        str (person_portrait ?sex ?size:(int o "size") f) );
    ( "url",
      fun f a ->
        let o = opts a in
        str (url ?width:(int o "width") ?height:(int o "height") f) );
    ( "urlLoremFlickr",
      fun f a ->
        let o = opts a in
        str
          (url_lorem_flickr ?width:(int o "width") ?height:(int o "height")
             ?category:(string o "category") f) );
    ( "urlPicsumPhotos",
      fun f a ->
        let o = opts a in
        str
          (url_picsum_photos ?width:(int o "width") ?height:(int o "height")
             ?grayscale:(bool o "grayscale") ?blur:(int o "blur") f) );
    ( "dataUri",
      fun f a ->
        let o = opts a in
        let type_ = Option.bind (string o "type") data_uri_type_of_string in
        str
          (data_uri ?width:(int o "width") ?height:(int o "height") ?color:(string o "color")
             ?type_ f) );
  ]
