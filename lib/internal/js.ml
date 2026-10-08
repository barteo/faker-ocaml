(* Small emulations of JavaScript built-ins whose exact output matters for
   seed-for-seed parity with faker-js. *)

(* ---------- Number.prototype.toString() ---------- *)

(* Shortest round-tripping decimal digits and decimal exponent of [x > 0]:
   x = 0.d1d2...dk * 10^n *)
let shortest_digits x =
  let rec go p =
    let s = Printf.sprintf "%.*e" p x in
    if p >= 16 || float_of_string s = x then s else go (p + 1)
  in
  let s = go 0 in
  let mantissa, exp =
    match String.index_opt s 'e' with
    | Some i ->
        (String.sub s 0 i, int_of_string (String.sub s (i + 1) (String.length s - i - 1)))
    | None -> (s, 0)
  in
  let digits = String.concat "" (String.split_on_char '.' mantissa) in
  (* strip trailing zeros *)
  let len = ref (String.length digits) in
  while !len > 1 && digits.[!len - 1] = '0' do
    decr len
  done;
  (String.sub digits 0 !len, exp + 1)

let rec number_to_string (x : float) =
  if Float.is_nan x then "NaN"
  else if x = 0.0 then "0"
  else if x < 0.0 then "-" ^ number_to_string (-.x)
  else if Float.is_integer x && x < 1e21 then Printf.sprintf "%.0f" x
  else if x = Float.infinity then "Infinity"
  else
    let s, n = shortest_digits x in
    let k = String.length s in
    if k <= n && n <= 21 then s ^ String.make (n - k) '0'
    else if 0 < n && n <= 21 then String.sub s 0 n ^ "." ^ String.sub s n (k - n)
    else if -6 < n && n <= 0 then "0." ^ String.make (-n) '0' ^ s
    else
      let e = n - 1 in
      let sign = if e < 0 then "-" else "+" in
      let e = abs e in
      if k = 1 then Printf.sprintf "%se%s%d" s sign e
      else Printf.sprintf "%c.%se%s%d" s.[0] (String.sub s 1 (k - 1)) sign e

let int_to_string = string_of_int

(* ---------- Number.prototype.toFixed() ---------- *)

(* Adds one to the decimal digit string [s] (no sign, no dot). *)
let increment_digits s =
  let b = Bytes.of_string s in
  let rec go i =
    if i < 0 then "1" ^ Bytes.to_string b
    else
      match Bytes.get b i with
      | '9' ->
          Bytes.set b i '0';
          go (i - 1)
      | ch ->
          Bytes.set b i (Char.chr (Char.code ch + 1));
          Bytes.to_string b
  in
  go (Bytes.length b - 1)

let to_fixed (x : float) (digits : int) =
  if Float.is_nan x then "NaN"
  else if Float.abs x >= 1e21 then number_to_string x
  else
    let neg = x < 0.0 in
    let ax = Float.abs x in
    (* Near-exact decimal expansion; JS rounds half away from zero on the exact value. *)
    let s = Printf.sprintf "%.*f" (digits + 25) ax in
    let ip, fp =
      match String.index_opt s '.' with
      | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
      | None -> (s, "")
    in
    let kept = String.sub fp 0 digits in
    let all = ip ^ kept in
    let all = if fp.[digits] >= '5' then increment_digits all else all in
    let il = String.length all - digits in
    let res =
      if digits = 0 then all else String.sub all 0 il ^ "." ^ String.sub all il digits
    in
    if neg then "-" ^ res else res

(* ---------- Number.prototype.toString(radix) for integers ---------- *)

let int_to_radix (n : int) (radix : int) =
  if n = 0 then "0"
  else
    let digits = "0123456789abcdefghijklmnopqrstuvwxyz" in
    let buf = Buffer.create 16 in
    let rec go n = if n > 0 then (go (n / radix); Buffer.add_char buf digits.[n mod radix]) in
    if n < 0 then (Buffer.add_char buf '-'; go (-n)) else go n;
    Buffer.contents buf

(* ---------- Number.parseInt(s) (radix 10) ---------- *)

let parse_int (s : string) : int option =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\n' || s.[!i] = '\r') do
    incr i
  done;
  let neg = !i < len && s.[!i] = '-' in
  if !i < len && (s.[!i] = '-' || s.[!i] = '+') then incr i;
  let start = !i in
  while !i < len && s.[!i] >= '0' && s.[!i] <= '9' do
    incr i
  done;
  if !i = start then None
  else
    let v = int_of_string (String.sub s start (!i - start)) in
    Some (if neg then -v else v)

(* ---------- String helpers ---------- *)

let repeat s n =
  if n <= 0 then ""
  else
    let b = Buffer.create (String.length s * n) in
    for _ = 1 to n do
      Buffer.add_string b s
    done;
    Buffer.contents b

let pad_start s len ch =
  let l = String.length s in
  if l >= len then s else String.make (len - l) ch ^ s

let pad_end s len ch =
  let l = String.length s in
  if l >= len then s else s ^ String.make (len - l) ch

(* Splits a UTF-8 string into its code points, each as a UTF-8 string
   (like [...str] in JavaScript). *)
let code_points (s : string) : string list =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      go (i + n) (String.sub s i n :: acc)
  in
  go 0 []

(* String.prototype.replaceAll(search, replacement) with literal strings. *)
let replace_all ~sub ~by s =
  if sub = "" then s
  else
    let b = Buffer.create (String.length s) in
    let sl = String.length sub in
    let rec go i =
      if i > String.length s - sl then Buffer.add_string b (String.sub s i (String.length s - i))
      else if String.sub s i sl = sub then (Buffer.add_string b by; go (i + sl))
      else (Buffer.add_char b s.[i]; go (i + 1))
    in
    go 0;
    Buffer.contents b

(* String.prototype.replace(search, replacement) with literal strings: first occurrence only. *)
let replace_first ~sub ~by s =
  let sl = String.length sub in
  let rec find i =
    if i > String.length s - sl then None
    else if String.sub s i sl = sub then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> s
  | Some i -> String.sub s 0 i ^ by ^ String.sub s (i + sl) (String.length s - i - sl)

let index_of ?(from = 0) ~sub s =
  let sl = String.length sub in
  let rec find i =
    if i > String.length s - sl then -1
    else if String.sub s i sl = sub then i
    else find (i + 1)
  in
  find (max 0 from)

let includes ~sub s = index_of ~sub s >= 0

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let ends_with ~suffix s =
  let ls = String.length s and lx = String.length suffix in
  ls >= lx && String.sub s (ls - lx) lx = suffix

(* JS substring(start, end) with clamping. *)
let substring s start end_ =
  let len = String.length s in
  let clamp x = max 0 (min len x) in
  let a = clamp start and b = clamp end_ in
  let a, b = if a > b then (b, a) else (a, b) in
  String.sub s a (b - a)

(* JS slice(start, end?) with negative index support. *)
let slice ?end_ s start =
  let len = String.length s in
  let norm x = if x < 0 then max 0 (len + x) else min len x in
  let a = norm start in
  let b = match end_ with None -> len | Some e -> norm e in
  if b <= a then "" else String.sub s a (b - a)

let capitalize s = String.capitalize_ascii s

(* ---------- floating point ---------- *)

(** Float multiplication that is never fused with a following addition.
    The arm64 backend turns [a *. b +. c] into a fused multiply-add, which
    rounds differently from JavaScript; always write [Js.mul a b +. c]. *)
let mul (a : float) (b : float) : float = Sys.opaque_identity (a *. b)
