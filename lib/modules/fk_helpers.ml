(* Port of src/modules/helpers/module.ts (SimpleHelpersModule) and
   src/modules/helpers/luhn-check.ts. [fake] lives in Fake (it needs the
   method registry). *)

let int9 f = Fk_number.int ~max:9 f

(* ---------- arrays ---------- *)

let array_element (arr : 'a array) f : 'a =
  let len = Array.length arr in
  if len = 0 then Core.error "Cannot get value from empty dataset.";
  let index = if len > 1 then Fk_number.int ~max:(len - 1) f else 0 in
  arr.(index)

let shuffle ?(inplace = false) (arr : 'a array) f : 'a array =
  let list = if inplace then arr else Array.copy arr in
  for i = Array.length list - 1 downto 1 do
    let j = Fk_number.int ~max:i f in
    let tmp = list.(i) in
    list.(i) <- list.(j);
    list.(j) <- tmp
  done;
  list

let range_to_number (range : Types.range) f =
  match range with `N n -> n | `Range (min, max) -> Fk_number.int ~min ~max f

let array_elements ?count (arr : 'a array) f : 'a array =
  let len = Array.length arr in
  if len = 0 then [||]
  else
    let num_elements =
      range_to_number
        (match count with Some c -> c | None -> `Range (1, len))
        f
    in
    if num_elements >= len then shuffle arr f
    else if num_elements <= 0 then [||]
    else
      let copy = Array.copy arr in
      let min = len - num_elements in
      for i = len - 1 downto min do
        let index = Fk_number.int ~max:i f in
        let tmp = copy.(index) in
        copy.(index) <- copy.(i);
        copy.(i) <- tmp
      done;
      Array.sub copy min num_elements

let weighted_array_element (arr : (float * 'a) array) f : 'a =
  if Array.length arr = 0 then
    Core.error "weightedArrayElement expects an array with at least one element";
  if Array.exists (fun (w, _) -> w <= 0.0) arr then
    Core.error
      "weightedArrayElement expects an array of { weight, value } objects \
       where weight is a positive number";
  let total = Array.fold_left (fun acc (w, _) -> acc +. w) 0.0 arr in
  let random = Fk_number.float ~min:0.0 ~max:total f in
  let rec go i current =
    if i >= Array.length arr then snd arr.(Array.length arr - 1)
    else
      let w, v = arr.(i) in
      let current = current +. w in
      if random < current then v else go (i + 1) current
  in
  go 0 0.0

let multiple ?(count = `N 3) (fn : int -> 'a) f : 'a array =
  let count = range_to_number count f in
  if count <= 0 then [||] else Array.init count fn

let unique_array (source : 'a array) length f : 'a array =
  let seen = Hashtbl.create 16 in
  let uniq =
    Array.of_list
      (List.filter
         (fun x ->
           if Hashtbl.mem seen x then false
           else (
             Hashtbl.add seen x ();
             true))
         (Array.to_list source))
  in
  let shuffled = shuffle uniq f in
  Array.sub shuffled 0 (min length (Array.length shuffled))

let unique_array_fn (source : unit -> 'a) length : 'a array =
  let seen = Hashtbl.create 16 in
  let out = ref [] in
  let max_attempts = 1000 * length in
  let attempts = ref 0 in
  (try
     while Hashtbl.length seen < length && !attempts < max_attempts do
       let v = source () in
       if not (Hashtbl.mem seen v) then (
         Hashtbl.add seen v ();
         out := v :: !out);
       incr attempts
     done
   with _ -> ());
  Array.of_list (List.rev !out)

let maybe ?probability (callback : unit -> 'a) f : 'a option =
  if Fk_datatype.boolean ?probability f then Some (callback ()) else None

let object_key (obj : (string * 'a) list) f : string =
  array_element (Array.of_list (List.map fst obj)) f

let object_value (obj : (string * 'a) list) f : 'a =
  List.assoc (object_key obj f) obj

let object_entry (obj : (string * 'a) list) f : string * 'a =
  let k = object_key obj f in
  (k, List.assoc k obj)

(** [enum_value enum f]: a random value of a TypeScript enum given as its
    object's [(key, value)] entries. Numeric keys (the reverse mappings
    TypeScript adds to numeric enums) are ignored. *)
let enum_value (enum : (string * 'a) list) f : 'a =
  let keys = List.filter (fun (k, _) -> Float.is_nan (Js.to_number k)) enum in
  let key = array_element (Array.of_list (List.map fst keys)) f in
  List.assoc key enum

(* ---------- strings ---------- *)

let slugify s =
  let s = Unicode.strip_combining_marks (Fk_internet_nfkd.nfkd s) in
  let s = Js.replace_all ~sub:" " ~by:"-" s in
  String.concat ""
    (List.filter_map
       (fun c ->
         match c with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '-' ->
             Some (String.make 1 c)
         | _ -> None)
       (List.init (String.length s) (String.get s)))

let alpha_upper = Array.init 26 (fun i -> String.make 1 (Char.chr (65 + i)))

let replace_symbols s f =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '#' -> Buffer.add_string b (string_of_int (int9 f))
      | '?' -> Buffer.add_string b (array_element alpha_upper f)
      | '*' ->
          Buffer.add_string b
            (if Fk_datatype.boolean f then array_element alpha_upper f
             else string_of_int (int9 f))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* Luhn (src/modules/helpers/luhn-check.ts) *)
let luhn_checksum str =
  let digits =
    List.filter
      (fun c -> not (c = ' ' || c = '-' || c = '\t' || c = '\n' || c = '\r'))
      (List.init (String.length str) (String.get str))
  in
  let sum = ref 0 and alternate = ref false in
  List.iter
    (fun c ->
      let n = Char.code c - 48 in
      let n =
        if !alternate then
          let n = n * 2 in
          if n > 9 then (n mod 10) + 1 else n
        else n
      in
      sum := !sum + n;
      alternate := not !alternate)
    (List.rev digits);
  !sum mod 10

let luhn_check str = luhn_checksum str = 0

let luhn_check_value str =
  let str =
    if Js.ends_with ~suffix:"L" str then
      String.sub str 0 (String.length str - 1) ^ "0"
    else str ^ "0"
  in
  let checksum = luhn_checksum str in
  if checksum = 0 then 0 else 10 - checksum

(* --- tiny matchers replacing the regular expressions used upstream --- *)

let is_digit c = c >= '0' && c <= '9'

let is_word c =
  is_digit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

(* Reads [\d+] at [i]; returns (value string, next index). *)
let read_digits s i =
  let j = ref i in
  while !j < String.length s && is_digit s.[!j] do
    incr j
  done;
  if !j = i then None else Some (String.sub s i (!j - i), !j)

(* /(.)\{(\d+),(\d+)\}/ and /(.)\{(\d+)\}/ *)
let find_range_rep s =
  let len = String.length s in
  let rec at i =
    if i + 1 >= len then None
    else if s.[i] <> '\n' && s.[i + 1] = '{' then
      match read_digits s (i + 2) with
      | Some (a, j) when j < len && s.[j] = ',' -> (
          match read_digits s (j + 1) with
          | Some (b, k) when k < len && s.[k] = '}' ->
              Some (i, k + 1, s.[i], a, b)
          | _ -> at (i + 1))
      | _ -> at (i + 1)
    else at (i + 1)
  in
  at 0

let find_rep s =
  let len = String.length s in
  let rec at i =
    if i + 1 >= len then None
    else if s.[i] <> '\n' && s.[i + 1] = '{' then
      match read_digits s (i + 2) with
      | Some (a, j) when j < len && s.[j] = '}' -> Some (i, j + 1, s.[i], a)
      | _ -> at (i + 1)
    else at (i + 1)
  in
  at 0

(* /\[(\d+)-(\d+)\]/ *)
let find_range s =
  let len = String.length s in
  let rec at i =
    if i >= len then None
    else if s.[i] = '[' then
      match read_digits s (i + 1) with
      | Some (a, j) when j < len && s.[j] = '-' -> (
          match read_digits s (j + 1) with
          | Some (b, k) when k < len && s.[k] = ']' -> Some (i, k + 1, a, b)
          | _ -> at (i + 1))
      | _ -> at (i + 1)
    else at (i + 1)
  in
  at 0

let splice s start stop repl =
  String.sub s 0 start ^ repl ^ String.sub s stop (String.length s - stop)

let pint s = Option.get (Js.parse_int s)

let legacy_regexp_string_parse s f =
  let s = ref s in
  let rec loop1 () =
    match find_range_rep !s with
    | Some (i, j, c, a, b) ->
        let min = pint a and max = pint b in
        let min, max = if min > max then (max, min) else (min, max) in
        let reps = Fk_number.int ~min ~max f in
        s := splice !s i j (Js.repeat (String.make 1 c) reps);
        loop1 ()
    | None -> ()
  in
  loop1 ();
  let rec loop2 () =
    match find_rep !s with
    | Some (i, j, c, a) ->
        s := splice !s i j (Js.repeat (String.make 1 c) (pint a));
        loop2 ()
    | None -> ()
  in
  loop2 ();
  let rec loop3 () =
    match find_range !s with
    | Some (i, j, a, b) ->
        let min = pint a and max = pint b in
        let min, max = if min > max then (max, min) else (min, max) in
        s := splice !s i j (string_of_int (Fk_number.int ~min ~max f));
        loop3 ()
    | None -> ()
  in
  loop3 ();
  !s

let legacy_replace_symbol_with_number ?(symbol = '#') s f =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if c = symbol then Buffer.add_string b (string_of_int (int9 f))
      else if c = '!' then
        Buffer.add_string b (string_of_int (Fk_number.int ~min:2 ~max:9 f))
      else Buffer.add_char b c)
    s;
  Buffer.contents b

(* Upstream defaults [s] to "6453-####-####-####-###L". *)
let replace_credit_card_symbols ?(symbol = '#') s f =
  let s = legacy_regexp_string_parse s f in
  let s = legacy_replace_symbol_with_number ~symbol s f in
  let check = luhn_check_value s in
  Js.replace_first ~sub:"L" ~by:(string_of_int check) s
