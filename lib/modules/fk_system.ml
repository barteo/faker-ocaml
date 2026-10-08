(* Port of src/modules/system/module.ts. *)

let common_file_types = [| "video"; "audio"; "image"; "text"; "application" |]

let common_mime_types =
  [|
    "application/pdf"; "audio/mpeg"; "audio/wav"; "image/png"; "image/jpeg"; "image/gif";
    "video/mp4"; "video/mpeg"; "text/html";
  |]

type interface_type = [ `En | `Wl | `Ww ]

let interface_type_to_string : interface_type -> string = function
  | `En -> "en"
  | `Wl -> "wl"
  | `Ww -> "ww"

let interface_type_of_string = function
  | "en" -> Some `En
  | "wl" -> Some `Wl
  | "ww" -> Some `Ww
  | _ -> None

type interface_schema = [ `Index | `Slot | `Mac | `Pci ]

let interface_schemas : (string * (interface_schema * string)) list =
  [ ("index", (`Index, "o")); ("slot", (`Slot, "s")); ("mac", (`Mac, "x")); ("pci", (`Pci, "p")) ]

let interface_schema_to_string (s : interface_schema) =
  fst (List.find (fun (_, (v, _)) -> v = s) interface_schemas)

let interface_schema_of_string s = Option.map fst (List.assoc_opt s interface_schemas)

let cron_day_of_week = [| "SUN"; "MON"; "TUE"; "WED"; "THU"; "FRI"; "SAT" |]

let mime_types f =
  match Locale.get f "system" "mime_type" with Json.Obj kvs -> kvs | _ -> []

let extensions_of entry =
  match Json.member "extensions" entry with Some v -> Locale.to_strings v | None -> [||]

(* new Set(...) then spread: first occurrence order. *)
let dedup (l : string list) =
  let seen = Hashtbl.create 64 in
  Array.of_list
    (List.filter
       (fun x ->
         if Hashtbl.mem seen x then false
         else (
           Hashtbl.add seen x ();
           true))
       l)

(* s.toLowerCase().replaceAll(/\W/g, '_'): every non-[A-Za-z0-9_] UTF-16 code unit
   becomes '_' (non-ASCII case mapping never yields ASCII word characters except
   for a few special cases not present in the word data). *)
let to_base_name s =
  Unicode.fold_uchars s (fun b cp raw ->
      if cp < 0x80 then
        let c = Char.lowercase_ascii raw.[0] in
        Buffer.add_char b (if Fk_helpers.is_word c then c else '_')
      else Buffer.add_string b (if cp >= 0x10000 then "__" else "_"))

(** [file_ext ?mime_type f]: an extension of [mime_type] (raises if unknown),
    or of any known MIME type. *)
let file_ext ?mime_type f =
  let mime_types = mime_types f in
  match mime_type with
  | Some m -> (
      match List.assoc_opt m mime_types with
      | Some entry when entry <> Json.Null -> Fk_helpers.array_element (extensions_of entry) f
      | _ -> Core.error "MIME type %s is not supported." m)
  | None ->
      let all = List.concat_map (fun (_, e) -> Array.to_list (extensions_of e)) mime_types in
      Fk_helpers.array_element (dedup all) f

(** [file_name ?extension_count f]: [extension_count] defaults to 1. *)
let file_name ?(extension_count = `N 1) f =
  let base_name = to_base_name (Fk_word.words f) in
  let extensions_suffix =
    String.concat "." (Array.to_list (Fk_helpers.multiple ~count:extension_count (fun _ -> file_ext f) f))
  in
  if extensions_suffix = "" then base_name else base_name ^ "." ^ extensions_suffix

let common_file_ext f = file_ext ~mime_type:(Fk_helpers.array_element common_mime_types f) f

(** [common_file_name ?extension f]: an empty [extension] counts as absent. *)
let common_file_name ?extension f =
  let file_name = file_name ~extension_count:(`N 0) f in
  let ext = match extension with Some e when e <> "" -> e | _ -> common_file_ext f in
  file_name ^ "." ^ ext

let mime_type f = Fk_helpers.array_element (Array.of_list (List.map fst (mime_types f))) f
let common_file_type f = Fk_helpers.array_element common_file_types f

let file_type f =
  let keys = List.map (fun (k, _) -> List.hd (String.split_on_char '/' k)) (mime_types f) in
  Fk_helpers.array_element (dedup keys) f

let directory_path f = Fk_helpers.array_element (Locale.strings f "system" "directory_path") f

let file_path f =
  let dir = directory_path f in
  dir ^ "/" ^ file_name f

let semver f =
  let a = Fk_number.int ~max:9 f in
  let b = Fk_number.int ~max:20 f in
  let c = Fk_number.int ~max:20 f in
  Printf.sprintf "%d.%d.%d" a b c

(** [network_interface ?interface_type ?interface_schema f]. *)
let network_interface ?(interface_type : interface_type option)
    ?(interface_schema : interface_schema option) f =
  let interface_type =
    match interface_type with
    | Some t -> t
    | None -> Fk_helpers.array_element [| `En; `Wl; `Ww |] f
  in
  let interface_schema =
    match interface_schema with
    | Some s -> s
    | None -> fst (List.assoc (Fk_helpers.object_key interface_schemas f) interface_schemas)
  in
  let numeric () = Fk_string.numeric f in
  let maybe_part p () =
    Option.value ~default:"" (Fk_helpers.maybe (fun () -> p ^ numeric ()) f)
  in
  let prefix, suffix =
    match interface_schema with
    | `Index -> ("", numeric ())
    | `Slot ->
        let a = numeric () in
        let b = maybe_part "f" () in
        let c = maybe_part "d" () in
        ("", a ^ b ^ c)
    | `Mac -> ("", Fk_internet.mac ~separator:"" f)
    | `Pci ->
        let prefix = maybe_part "P" () in
        let a = numeric () in
        let b = numeric () in
        let c = maybe_part "f" () in
        let d = maybe_part "d" () in
        (prefix, a ^ "s" ^ b ^ c ^ d)
  in
  prefix ^ interface_type_to_string interface_type
  ^ snd (List.assoc (interface_schema_to_string interface_schema) interface_schemas)
  ^ suffix

(** [cron ?include_year ?include_non_standard f]. *)
let cron ?(include_year = false) ?(include_non_standard = false) f =
  let int ?min ~max () = string_of_int (Fk_number.int ?min ~max f) in
  let minutes = [| int ~max:59 (); "*" |] in
  let hours = [| int ~max:23 (); "*" |] in
  let days = [| int ~min:1 ~max:31 (); "*"; "?" |] in
  let months = [| int ~min:1 ~max:12 (); "*" |] in
  let dow_num = int ~max:6 () in
  let dow_name = Fk_helpers.array_element cron_day_of_week f in
  let days_of_week = [| dow_num; dow_name; "*"; "?" |] in
  let years = [| int ~min:1970 ~max:2099 (); "*" |] in
  let minute = Fk_helpers.array_element minutes f in
  let hour = Fk_helpers.array_element hours f in
  let day = Fk_helpers.array_element days f in
  let month = Fk_helpers.array_element months f in
  let day_of_week = Fk_helpers.array_element days_of_week f in
  let year = Fk_helpers.array_element years f in
  let standard = String.concat " " [ minute; hour; day; month; day_of_week ] in
  let standard = if include_year then standard ^ " " ^ year else standard in
  let non_standard =
    [| "@annually"; "@daily"; "@hourly"; "@monthly"; "@reboot"; "@weekly"; "@yearly" |]
  in
  if (not include_non_standard) || Fk_datatype.boolean f then standard
  else Fk_helpers.array_element non_standard f

let registry : (string * Registry.fn) list =
  let open Args in
  let s g = fun f _ -> str (g f) in
  [
    ( "fileName",
      fun f a -> let o = opts a in str (file_name ?extension_count:(range o "extensionCount") f) );
    ( "commonFileName",
      fun f a ->
        let extension = match nth a 0 with Some Json.Null | None -> None | Some v -> Some (Json.to_js_string v) in
        str (common_file_name ?extension f) );
    ("mimeType", s mime_type);
    ("commonFileType", s common_file_type);
    ("commonFileExt", s common_file_ext);
    ("fileType", s file_type);
    ( "fileExt",
      fun f a ->
        let mime_type = match nth a 0 with Some (Json.Str m) -> Some m | _ -> None in
        str (file_ext ?mime_type f) );
    ("directoryPath", s directory_path);
    ("filePath", s file_path);
    ("semver", s semver);
    ( "networkInterface",
      fun f a ->
        let o = opts a in
        str
          (network_interface
             ?interface_type:(Option.bind (string o "interfaceType") interface_type_of_string)
             ?interface_schema:(Option.bind (string o "interfaceSchema") interface_schema_of_string)
             f) );
    ( "cron",
      fun f a ->
        let o = opts a in
        str
          (cron ?include_year:(bool o "includeYear")
             ?include_non_standard:(bool o "includeNonStandard") f) );
  ]
