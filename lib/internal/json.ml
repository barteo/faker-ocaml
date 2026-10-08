(* Minimal JSON values: used for locale definitions, helpers.fake() call
   parameters and JSON.stringify-compatible output. *)

type t =
  | Null
  | Bool of bool
  | Num of float
  | Str of string
  | Arr of t array
  | Obj of (string * t) list

exception Parse_error of string

(* ---------- parsing ---------- *)

let add_utf8 b cp = Buffer.add_utf_8_uchar b (Uchar.of_int cp)

let parse (s : string) : t =
  let len = String.length s in
  let pos = ref 0 in
  let fail msg = raise (Parse_error (Printf.sprintf "%s at %d" msg !pos)) in
  let peek () = if !pos < len then s.[!pos] else '\000' in
  let rec ws () =
    if !pos < len then
      match s.[!pos] with
      | ' ' | '\t' | '\n' | '\r' ->
          incr pos;
          ws ()
      | _ -> ()
  in
  let expect c = if peek () = c then incr pos else fail (Printf.sprintf "expected '%c'" c) in
  let literal word v =
    let l = String.length word in
    if !pos + l <= len && String.sub s !pos l = word then (pos := !pos + l; v)
    else fail "invalid literal"
  in
  let hex4 () =
    if !pos + 4 > len then fail "bad unicode escape";
    let v = int_of_string ("0x" ^ String.sub s !pos 4) in
    pos := !pos + 4;
    v
  in
  let string () =
    expect '"';
    let b = Buffer.create 16 in
    let rec go () =
      if !pos >= len then fail "unterminated string";
      let c = s.[!pos] in
      incr pos;
      match c with
      | '"' -> ()
      | '\\' ->
          let e = peek () in
          incr pos;
          (match e with
          | '"' -> Buffer.add_char b '"'
          | '\\' -> Buffer.add_char b '\\'
          | '/' -> Buffer.add_char b '/'
          | 'b' -> Buffer.add_char b '\b'
          | 'f' -> Buffer.add_char b '\012'
          | 'n' -> Buffer.add_char b '\n'
          | 'r' -> Buffer.add_char b '\r'
          | 't' -> Buffer.add_char b '\t'
          | 'u' ->
              let cp = hex4 () in
              if cp >= 0xD800 && cp <= 0xDBFF && !pos + 1 < len && s.[!pos] = '\\'
                 && s.[!pos + 1] = 'u'
              then begin
                pos := !pos + 2;
                let lo = hex4 () in
                add_utf8 b (0x10000 + ((cp - 0xD800) lsl 10) + (lo - 0xDC00))
              end
              else add_utf8 b cp
          | _ -> fail "bad escape");
          go ()
      | c when Char.code c < 0x20 -> fail "control character in string"
      | c ->
          Buffer.add_char b c;
          go ()
    in
    go ();
    Buffer.contents b
  in
  let number () =
    let start = !pos in
    let is_num c =
      match c with '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true | _ -> false
    in
    while !pos < len && is_num s.[!pos] do
      incr pos
    done;
    match float_of_string_opt (String.sub s start (!pos - start)) with
    | Some f -> Num f
    | None -> fail "bad number"
  in
  let rec value () =
    ws ();
    match peek () with
    | '{' ->
        incr pos;
        ws ();
        if peek () = '}' then (incr pos; Obj [])
        else
          let rec members acc =
            ws ();
            let k = string () in
            ws ();
            expect ':';
            let v = value () in
            ws ();
            match peek () with
            | ',' ->
                incr pos;
                members ((k, v) :: acc)
            | '}' ->
                incr pos;
                Obj (List.rev ((k, v) :: acc))
            | _ -> fail "expected ',' or '}'"
          in
          members []
    | '[' ->
        incr pos;
        ws ();
        if peek () = ']' then (incr pos; Arr [||])
        else
          let rec elems acc =
            let v = value () in
            ws ();
            match peek () with
            | ',' ->
                incr pos;
                elems (v :: acc)
            | ']' ->
                incr pos;
                Arr (Array.of_list (List.rev (v :: acc)))
            | _ -> fail "expected ',' or ']'"
          in
          elems []
    | '"' -> Str (string ())
    | 't' -> literal "true" (Bool true)
    | 'f' -> literal "false" (Bool false)
    | 'n' -> literal "null" Null
    | '-' | '0' .. '9' -> number ()
    | _ -> fail "unexpected character"
  in
  let v = value () in
  ws ();
  if !pos <> len then fail "trailing characters";
  v

(* ---------- JSON.stringify ---------- *)

let escape_string_to b s =
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\b' -> Buffer.add_string b "\\b"
      | '\012' -> Buffer.add_string b "\\f"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"'

let quote s =
  let b = Buffer.create (String.length s + 2) in
  escape_string_to b s;
  Buffer.contents b

let rec to_buffer b = function
  | Null -> Buffer.add_string b "null"
  | Bool v -> Buffer.add_string b (if v then "true" else "false")
  | Num f ->
      Buffer.add_string b (if Float.is_finite f then Js.number_to_string f else "null")
  | Str s -> escape_string_to b s
  | Arr a ->
      Buffer.add_char b '[';
      Array.iteri
        (fun i v ->
          if i > 0 then Buffer.add_char b ',';
          to_buffer b v)
        a;
      Buffer.add_char b ']'
  | Obj kvs ->
      Buffer.add_char b '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_char b ',';
          escape_string_to b k;
          Buffer.add_char b ':';
          to_buffer b v)
        kvs;
      Buffer.add_char b '}'

let to_string v =
  let b = Buffer.create 64 in
  to_buffer b v;
  Buffer.contents b

(* String(value) in JavaScript. *)
let rec to_js_string = function
  | Null -> "null"
  | Bool v -> if v then "true" else "false"
  | Num f -> Js.number_to_string f
  | Str s -> s
  | Arr a ->
      String.concat ","
        (Array.to_list (Array.map (function Null -> "" | v -> to_js_string v) a))
  | Obj _ -> "[object Object]"

(* ---------- accessors ---------- *)

let member k = function Obj kvs -> List.assoc_opt k kvs | _ -> None

let int i = Num (float_of_int i)
let str s = Str s
let strs l = Arr (Array.of_list (List.map str l))
let list f l = Arr (Array.of_list (List.map f l))
