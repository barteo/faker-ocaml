(* Port of src/modules/internet/module.ts (+ char-mappings.ts, see
   Fk_internet_char_mappings) and src/internal/base64.ts.

   OCaml-specific API notes:
   - [password ?pattern] takes a predicate [char -> bool] instead of a RegExp;
     it is applied to each candidate character (code points 33..127, all
     ASCII). The default is upstream's [/\w/] ([A-Za-z0-9_]).
   - [jwt ?header ?payload] take JSON objects ([Json.t]); they are serialized
     like JSON.stringify (key order as given; JS would move integer-like keys
     first, which is not emulated).
   - Dates are float epoch milliseconds. *)

type emoji_type =
  [ `Smiley | `Body | `Person | `Nature | `Food | `Travel | `Activity | `Object | `Symbol | `Flag ]

let emoji_type_to_string : emoji_type -> string = function
  | `Smiley -> "smiley"
  | `Body -> "body"
  | `Person -> "person"
  | `Nature -> "nature"
  | `Food -> "food"
  | `Travel -> "travel"
  | `Activity -> "activity"
  | `Object -> "object"
  | `Symbol -> "symbol"
  | `Flag -> "flag"

type http_status_code_type =
  [ `Informational | `Success | `Client_error | `Server_error | `Redirection ]

let http_status_code_type_to_string : http_status_code_type -> string = function
  | `Informational -> "informational"
  | `Success -> "success"
  | `Client_error -> "clientError"
  | `Server_error -> "serverError"
  | `Redirection -> "redirection"

type http_protocol = [ `Http | `Https ]

let http_protocol_to_string : http_protocol -> string = function
  | `Http -> "http"
  | `Https -> "https"

type http_method = [ `GET | `POST | `PUT | `DELETE | `PATCH ]

let http_method_to_string : http_method -> string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"

(** [IPv4Network] presets. *)
type ipv4_network =
  [ `Any
  | `Loopback
  | `Private_a
  | `Private_b
  | `Private_c
  | `Test_net_1
  | `Test_net_2
  | `Test_net_3
  | `Link_local
  | `Multicast ]

let ipv4_networks : (string * (ipv4_network * string)) list =
  [
    ("any", (`Any, "0.0.0.0/0"));
    ("loopback", (`Loopback, "127.0.0.0/8"));
    ("private-a", (`Private_a, "10.0.0.0/8"));
    ("private-b", (`Private_b, "172.16.0.0/12"));
    ("private-c", (`Private_c, "192.168.0.0/16"));
    ("test-net-1", (`Test_net_1, "192.0.2.0/24"));
    ("test-net-2", (`Test_net_2, "198.51.100.0/24"));
    ("test-net-3", (`Test_net_3, "203.0.113.0/24"));
    ("link-local", (`Link_local, "169.254.0.0/16"));
    ("multicast", (`Multicast, "224.0.0.0/4"));
  ]

let ipv4_network_cidr (n : ipv4_network) =
  snd (snd (List.find (fun (_, (m, _)) -> m = n) ipv4_networks))

let ipv4_network_to_string (n : ipv4_network) = fst (List.find (fun (_, (m, _)) -> m = n) ipv4_networks)

let ipv4_network_of_string s = Option.map fst (List.assoc_opt s ipv4_networks)

(* ---------- helpers ---------- *)

let el entry f = Fk_helpers.array_element (Locale.strings f "internet" entry) f
let obj_keys = function Json.Obj kvs -> Array.of_list (List.map fst kvs) | _ -> [||]

(* Bytes-level filter, valid on UTF-8 when [keep] only accepts ASCII bytes. *)
let filter_bytes keep s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> if keep c then Buffer.add_char b c) s;
  Buffer.contents b

let remove_quotes_and_spaces s = filter_bytes (fun c -> c <> '\'' && c <> ' ') s

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'

(* ---------- base64url (src/internal/base64.ts) ---------- *)

let to_base64_url (input : string) =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" in
  let len = String.length input in
  let b = Buffer.create ((len + 2) / 3 * 4) in
  let byte i = Char.code input.[i] in
  let rec go i =
    if i + 2 < len then begin
      let n = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      Buffer.add_char b alphabet.[(n lsr 18) land 63];
      Buffer.add_char b alphabet.[(n lsr 12) land 63];
      Buffer.add_char b alphabet.[(n lsr 6) land 63];
      Buffer.add_char b alphabet.[n land 63];
      go (i + 3)
    end
    else if i + 1 < len then begin
      let n = (byte i lsl 16) lor (byte (i + 1) lsl 8) in
      Buffer.add_char b alphabet.[(n lsr 18) land 63];
      Buffer.add_char b alphabet.[(n lsr 12) land 63];
      Buffer.add_char b alphabet.[(n lsr 6) land 63]
    end
    else if i < len then begin
      let n = byte i lsl 16 in
      Buffer.add_char b alphabet.[(n lsr 18) land 63];
      Buffer.add_char b alphabet.[(n lsr 12) land 63]
    end
  in
  go 0;
  Buffer.contents b

(* ---------- names ---------- *)

(** [username ?first_name ?last_name f]: names default to random person names;
    non-ASCII characters are transliterated (NFKD, char mappings, else the
    base-36 code point). *)
let username ?first_name ?last_name f =
  let has_last_name = match last_name with Some s -> s <> "" | None -> false in
  let first_name = match first_name with Some v -> v | None -> Fk_person.first_name f in
  let last_name = match last_name with Some v -> v | None -> Fk_person.last_name f in
  let separator = Fk_helpers.array_element [| "."; "_" |] f in
  let disambiguator = string_of_int (Fk_number.int ~max:99 f) in
  let strategies =
    [
      (fun () -> first_name ^ separator ^ last_name ^ disambiguator);
      (fun () -> first_name ^ separator ^ last_name);
    ]
    @ if not has_last_name then [ (fun () -> first_name ^ disambiguator) ] else []
  in
  let result = (Fk_helpers.array_element (Array.of_list strategies) f) () in
  let result = Unicode.strip_combining_marks (Fk_internet_nfkd.nfkd result) in
  let result =
    Unicode.fold_uchars result (fun b cp raw ->
        match Fk_internet_char_mappings.lookup cp with
        | Some m -> Buffer.add_string b m
        | None -> if cp < 0x80 then Buffer.add_string b raw else Buffer.add_string b (Js.int_to_radix cp 36))
  in
  remove_quotes_and_spaces result

(** [email ?first_name ?last_name ?provider ?allow_special_characters f]. *)
let email ?first_name ?last_name ?provider ?(allow_special_characters = false) f =
  let provider = match provider with Some p -> p | None -> el "free_email" f in
  let local_part = username ?first_name ?last_name f in
  (* /[^A-Za-z0-9._+-]+/g *)
  let local_part =
    filter_bytes (fun c -> is_alpha c || is_digit c || c = '.' || c = '_' || c = '+' || c = '-') local_part
  in
  let local_part = Js.substring local_part 0 50 in
  let local_part =
    if allow_special_characters then
      let username_chars = [| "."; "_"; "-" |] in
      let special_chars = Array.of_list (Js.code_points ".!#$%&'*+-/=?^_`{|}~") in
      let sub = Fk_helpers.array_element username_chars f in
      let by = Fk_helpers.array_element special_chars f in
      Js.replace_first ~sub ~by local_part
    else local_part
  in
  (* /\.{2,}/g -> '.' *)
  let b = Buffer.create (String.length local_part) in
  String.iteri
    (fun i c -> if not (c = '.' && i > 0 && local_part.[i - 1] = '.') then Buffer.add_char b c)
    local_part;
  let local_part = Buffer.contents b in
  let local_part = if Js.starts_with ~prefix:"." local_part then Js.slice local_part 1 else local_part in
  let local_part =
    if Js.ends_with ~suffix:"." local_part then Js.slice ~end_:(-1) local_part 0 else local_part
  in
  local_part ^ "@" ^ provider

(** [example_email ?first_name ?last_name ?allow_special_characters f]: an
    email at a reserved example domain. *)
let example_email ?first_name ?last_name ?allow_special_characters f =
  let provider = el "example_email" f in
  email ?first_name ?last_name ~provider ?allow_special_characters f

(** [display_name ?first_name ?last_name f]. *)
let display_name ?first_name ?last_name f =
  let first_name = match first_name with Some v -> v | None -> Fk_person.first_name f in
  let last_name = match last_name with Some v -> v | None -> Fk_person.last_name f in
  let separator = Fk_helpers.array_element [| "."; "_" |] f in
  let disambiguator = string_of_int (Fk_number.int ~max:99 f) in
  let strategies =
    [|
      (fun () -> first_name ^ disambiguator);
      (fun () -> first_name ^ separator ^ last_name);
      (fun () -> first_name ^ separator ^ last_name ^ disambiguator);
    |]
  in
  remove_quotes_and_spaces ((Fk_helpers.array_element strategies f) ())

(* ---------- web ---------- *)

let protocol f : http_protocol = Fk_helpers.array_element [| `Http; `Https |] f

let http_method f : http_method = Fk_helpers.array_element [| `GET; `POST; `PUT; `DELETE; `PATCH |] f

let http_status_code_of_strings types f =
  let data = Locale.get f "internet" "http_status_code" in
  let types = match types with Some t -> t | None -> obj_keys data in
  let typ = Fk_helpers.array_element types f in
  match Json.member typ data with
  | Some (Json.Arr codes) -> int_of_float (Locale.num (Fk_helpers.array_element codes f))
  | _ -> Core.error "Cannot read properties of undefined (reading 'length')"

(** [http_status_code ?types f]: [types] defaults to all categories. *)
let http_status_code ?(types : http_status_code_type list option) f =
  http_status_code_of_strings
    (Option.map (fun l -> Array.of_list (List.map http_status_code_type_to_string l)) types)
    f

let domain_suffix f = el "domain_suffix" f

(* /^[a-z][a-z-]*[a-z]$/i *)
let is_valid_domain_word_slug slug =
  let n = String.length slug in
  n >= 2
  && is_alpha slug.[0]
  && is_alpha slug.[n - 1]
  && String.for_all (fun c -> is_alpha c || c = '-') slug

let make_valid_domain_word_slug f word =
  let slug1 = Fk_helpers.slugify word in
  if is_valid_domain_word_slug slug1 then slug1
  else
    let slug2 = Fk_helpers.slugify (Fk_lorem.word f) in
    if is_valid_domain_word_slug slug2 then slug2
    else
      let length = Fk_number.int ~min:4 ~max:8 f in
      Fk_string.alpha ~casing:`Lower ~length:(`N length) f

let domain_word f =
  let word1 = make_valid_domain_word_slug f (Fk_word.adjective f) in
  let word2 = make_valid_domain_word_slug f (Fk_word.noun f) in
  String.lowercase_ascii (word1 ^ "-" ^ word2)

let domain_name f =
  let w = domain_word f in
  w ^ "." ^ domain_suffix f

(** [url ?append_slash ?protocol f]: [append_slash] defaults to random,
    [protocol] to [`Https]. *)
let url ?append_slash ?(protocol : http_protocol = `Https) f =
  let append_slash = match append_slash with Some b -> b | None -> Fk_datatype.boolean f in
  http_protocol_to_string protocol ^ "://" ^ domain_name f ^ if append_slash then "/" else ""

(* ---------- ip ---------- *)

(* /^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\/\d{1,2}$/ *)
let valid_cidr s =
  let n = String.length s in
  let rec digits i k max = if k < max && i < n && is_digit s.[i] then digits (i + 1) (k + 1) max else (i, k) in
  let rec octets i count =
    let j, k = digits i 0 3 in
    if k = 0 then false
    else if count < 3 then j < n && s.[j] = '.' && octets (j + 1) (count + 1)
    else
      j < n
      && s.[j] = '/'
      &&
      let e, k2 = digits (j + 1) 0 2 in
      k2 > 0 && e = n
  in
  octets 0 0

let ipv4_of_cidr cidr_block f =
  if not (valid_cidr cidr_block) then
    Core.error "Invalid CIDR block provided: %s. Must be in the format x.x.x.x/y." cidr_block;
  let ip_text, subnet =
    match String.split_on_char '/' cidr_block with [ a; b ] -> (a, b) | _ -> assert false
  in
  let subnet_value = int_of_string subnet in
  if subnet_value > 32 then
    Core.error "Invalid CIDR block provided: %s. Prefix length must be between 0 and 32." cidr_block;
  let octets = List.map int_of_string (String.split_on_char '.' ip_text) in
  if List.exists (fun o -> o > 255) octets then
    Core.error "Invalid CIDR block provided: %s. Each octet must be between 0 and 255." cidr_block;
  if subnet_value = 32 then ip_text
  else
    let subnet_mask = 0xffffffff lsr subnet_value in
    let raw_ip = List.fold_left (fun acc o -> (acc lsl 8) lor o) 0 octets land 0xffffffff in
    let network_ip = raw_ip land lnot subnet_mask land 0xffffffff in
    let host_offset = Fk_number.int ~max:subnet_mask f in
    let ip = (network_ip lor host_offset) land 0xffffffff in
    String.concat "."
      (List.map string_of_int
         [ (ip lsr 24) land 0xff; (ip lsr 16) land 0xff; (ip lsr 8) land 0xff; ip land 0xff ])

(** [ipv4 ?cidr_block ?network f]: [cidr_block] (e.g. ["192.168.0.0/16"])
    takes precedence over [network] (default [`Any]). *)
let ipv4 ?cidr_block ?(network : ipv4_network = `Any) f =
  let cidr_block = match cidr_block with Some c -> c | None -> ipv4_network_cidr network in
  ipv4_of_cidr cidr_block f

let ipv6 f =
  String.concat ":"
    (List.init 8 (fun _ -> Fk_string.hexadecimal ~length:(`N 4) ~casing:`Lower ~prefix:"" f))

let ip f = if Fk_datatype.boolean f then ipv4 f else ipv6 f
let port f = Fk_number.int ~min:1 ~max:65535 f
let user_agent f = Fake.fake_json (Locale.get f "internet" "user_agent_pattern") f

(** [mac ?separator f]: [separator] must be [":"], ["-"] or [""], otherwise
    [":"] is used. *)
let mac ?(separator = ":") f =
  let separator = if List.mem separator [ ":"; "-"; "" ] then separator else ":" in
  let b = Buffer.create 17 in
  for i = 0 to 11 do
    Buffer.add_string b (Fk_number.hex ~max:15 f);
    if i mod 2 = 1 && i <> 11 then Buffer.add_string b separator
  done;
  Buffer.contents b

(** [password ?length ?memorable ?pattern ?prefix f]: [length] defaults to
    15, [pattern] (a predicate standing in for upstream's RegExp) to [/\w/].
    Like upstream, never terminates if [pattern] rejects every character in
    ['!'..'\127']. *)
let password ?(length = 15) ?(memorable = false) ?(pattern = Fk_helpers.is_word) ?(prefix = "") f =
  let is_vowel c = String.contains "aeiouAEIOU" c in
  let is_consonant c = is_alpha c && not (is_vowel c) in
  let result = Buffer.create (max 0 length) in
  Buffer.add_string result prefix;
  let len = ref (Unicode.js_length prefix) in
  let current_pattern = ref pattern in
  while !len < length do
    if memorable then begin
      let n = Buffer.length result in
      let last_consonant = n > 0 && is_consonant (Buffer.nth result (n - 1)) in
      current_pattern := if last_consonant then is_vowel else is_consonant
    end;
    let n = Fk_number.int ~max:94 f + 33 in
    let c = Char.chr n in
    let c = if memorable then Char.lowercase_ascii c else c in
    if !current_pattern c then begin
      Buffer.add_char result c;
      incr len
    end
  done;
  Buffer.contents result

let emoji_of_strings types f =
  let data = Locale.get f "internet" "emoji" in
  let types = match types with Some t -> t | None -> obj_keys data in
  let typ = Fk_helpers.array_element types f in
  match Json.member typ data with
  | Some (Json.Arr a) -> Locale.to_string (Fk_helpers.array_element a f)
  | _ -> Core.error "Cannot read properties of undefined (reading 'length')"

(** [emoji ?types f]: [types] defaults to all categories. *)
let emoji ?(types : emoji_type list option) f =
  emoji_of_strings (Option.map (fun l -> Array.of_list (List.map emoji_type_to_string l)) types) f

let jwt_algorithm f = el "jwt_algorithm" f

let jwt_input ?header ?payload ?ref_date f =
  let ref_date = Fk_date.ref_input ref_date f in
  let iat_default = Fk_date.recent_input ~ref_date f in
  let round_s ms = Json.Num (Float.floor ((ms /. 1000.0) +. 0.5)) in
  let header =
    match header with
    | Some h -> h
    | None ->
        let alg = jwt_algorithm f in
        Json.Obj [ ("alg", Json.Str alg); ("typ", Json.Str "JWT") ]
  in
  let payload =
    match payload with
    | Some p -> p
    | None ->
        let iat = round_s iat_default in
        let exp = round_s (Fk_date.soon_input ~ref_date:(`Date iat_default) f) in
        let nbf = round_s (Fk_date.anytime_input ~ref_date f) in
        let iss = Fk_company.name f in
        let sub = Fk_string.uuid f in
        let aud = Fk_string.uuid f in
        let jti = Fk_string.uuid f in
        Json.Obj
          [
            ("iat", iat);
            ("exp", exp);
            ("nbf", nbf);
            ("iss", Json.Str iss);
            ("sub", Json.Str sub);
            ("aud", Json.Str aud);
            ("jti", Json.Str jti);
          ]
  in
  let encoded_header = to_base64_url (Json.to_string header) in
  let encoded_payload = to_base64_url (Json.to_string payload) in
  let signature = Fk_string.alphanumeric ~length:(`N 64) f in
  encoded_header ^ "." ^ encoded_payload ^ "." ^ signature

(** [jwt ?header ?payload ?ref_date f]: a random JSON Web Token; [header] and
    [payload] are JSON objects replacing the random defaults. *)
let jwt ?header ?payload ?ref_date f =
  jwt_input ?header ?payload ?ref_date:(Fk_date.num_opt ref_date) f

(* ---------- registry ---------- *)

let registry : (string * Registry.fn) list =
  let open Args in
  let s g = fun f _ -> str (g f) in
  let names o = (string o "firstName", string o "lastName") in
  let str_array o key =
    match get o key with
    | Some (Json.Arr a) -> Some (Array.map Json.to_js_string a)
    | _ -> None
  in
  let obj o key = match List.assoc_opt key o with None -> None | v -> v in
  [
    ( "email",
      fun f a ->
        let o = opts a in
        let first_name, last_name = names o in
        str
          (email ?first_name ?last_name ?provider:(string o "provider")
             ?allow_special_characters:(bool o "allowSpecialCharacters") f) );
    ( "exampleEmail",
      fun f a ->
        let o = opts a in
        let first_name, last_name = names o in
        str
          (example_email ?first_name ?last_name
             ?allow_special_characters:(bool o "allowSpecialCharacters") f) );
    ( "username",
      fun f a ->
        let o = opts a in
        let first_name, last_name = names o in
        str (username ?first_name ?last_name f) );
    ( "displayName",
      fun f a ->
        let o = opts a in
        let first_name, last_name = names o in
        str (display_name ?first_name ?last_name f) );
    ("protocol", fun f _ -> str (http_protocol_to_string (protocol f)));
    ("httpMethod", fun f _ -> str (http_method_to_string (http_method f)));
    ( "httpStatusCode",
      fun f a -> let o = opts a in int_ (http_status_code_of_strings (str_array o "types") f) );
    ( "url",
      fun f a ->
        let o = opts a in
        let protocol = match string o "protocol" with Some "http" -> Some `Http | _ -> None in
        str (url ?append_slash:(bool o "appendSlash") ?protocol f) );
    ("domainName", s domain_name);
    ("domainSuffix", s domain_suffix);
    ("domainWord", s domain_word);
    ("ip", s ip);
    ( "ipv4",
      fun f a ->
        let o = opts a in
        let cidr =
          match string o "cidrBlock" with
          | Some c -> c
          | None -> (
              match string o "network" with
              | None -> ipv4_network_cidr `Any
              | Some n -> (
                  match ipv4_network_of_string n with
                  | Some n -> ipv4_network_cidr n
                  | None -> "undefined"))
        in
        str (ipv4_of_cidr cidr f) );
    ("ipv6", s ipv6);
    ("port", fun f _ -> int_ (port f));
    ("userAgent", s user_agent);
    ( "mac",
      fun f a ->
        let o = opts ~shorthand:"separator" a in
        str (mac ?separator:(Option.map Json.to_js_string (get o "separator")) f) );
    ( "password",
      fun f a ->
        let o = opts a in
        (* a RegExp cannot be expressed in JSON: any other value has no .test() *)
        let pattern =
          match get o "pattern" with
          | None -> None
          | Some _ -> Some (fun _ -> Core.error "currentPattern.test is not a function")
        in
        str
          (password ?length:(int o "length") ?memorable:(bool o "memorable") ?pattern
             ?prefix:(string o "prefix") f) );
    ("emoji", fun f a -> let o = opts a in str (emoji_of_strings (str_array o "types") f));
    ("jwtAlgorithm", s jwt_algorithm);
    ( "jwt",
      fun f a ->
        let o = opts a in
        str (jwt_input ?header:(obj o "header") ?payload:(obj o "payload") ?ref_date:(Fk_date.ref_arg o) f)
    );
  ]
