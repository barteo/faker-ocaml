(* Arbitrary-precision integers with JavaScript BigInt semantics, for
   number.bigInt. A value is a sign and a magnitude: a little-endian array of
   base-10^4 limbs without high zero limbs. Zero is the empty magnitude and is
   never negative. *)

type t = { neg : bool; mag : int array }

let base = 10_000
let zero = { neg = false; mag = [||] }

let make neg mag =
  let n = ref (Array.length mag) in
  while !n > 0 && mag.(!n - 1) = 0 do
    decr n
  done;
  let mag = if !n = Array.length mag then mag else Array.sub mag 0 !n in
  { neg = neg && !n > 0; mag }

(* Works on the negative side too, so [min_int] doesn't overflow. *)
let of_int n =
  let rec limbs n acc =
    if n = 0 then List.rev acc else limbs (n / base) (abs (n mod base) :: acc)
  in
  make (n < 0) (Array.of_list (limbs n []))

let one = of_int 1

(* ---------- magnitudes ---------- *)

let cmp_mag a b =
  let la = Array.length a and lb = Array.length b in
  if la <> lb then compare la lb
  else
    let rec go i =
      if i < 0 then 0
      else if a.(i) <> b.(i) then compare a.(i) b.(i)
      else go (i - 1)
    in
    go (la - 1)

let add_mag a b =
  let la = Array.length a and lb = Array.length b in
  let r = Array.make (max la lb + 1) 0 in
  let carry = ref 0 in
  for i = 0 to Array.length r - 1 do
    let s =
      (if i < la then a.(i) else 0) + (if i < lb then b.(i) else 0) + !carry
    in
    r.(i) <- s mod base;
    carry := s / base
  done;
  r

(* Requires [a >= b]. *)
let sub_mag a b =
  let lb = Array.length b in
  let r = Array.copy a in
  let borrow = ref 0 in
  for i = 0 to Array.length r - 1 do
    let s = r.(i) - (if i < lb then b.(i) else 0) - !borrow in
    if s < 0 then (
      r.(i) <- s + base;
      borrow := 1)
    else (
      r.(i) <- s;
      borrow := 0)
  done;
  r

let mul_mag a b =
  let la = Array.length a and lb = Array.length b in
  let r = Array.make (la + lb + 1) 0 in
  for i = 0 to la - 1 do
    let carry = ref 0 in
    for j = 0 to lb - 1 do
      let s = r.(i + j) + (a.(i) * b.(j)) + !carry in
      r.(i + j) <- s mod base;
      carry := s / base
    done;
    let k = ref (i + lb) in
    while !carry > 0 do
      let s = r.(!k) + !carry in
      r.(!k) <- s mod base;
      carry := s / base;
      incr k
    done
  done;
  r

(* [a * m + c] for small [m] and [c]. *)
let mul_add_small a m c =
  let r = Array.make (Array.length a + 2) 0 in
  let carry = ref c in
  Array.iteri
    (fun i x ->
      let s = (x * m) + !carry in
      r.(i) <- s mod base;
      carry := s / base)
    a;
  r.(Array.length a) <- !carry mod base;
  r.(Array.length a + 1) <- !carry / base;
  (make false r).mag

(* Schoolbook long division, one limb of the quotient at a time; each limb is
   found by binary search. Returns (quotient, remainder). *)
let divmod_mag a b =
  let q = Array.make (Array.length a) 0 in
  let r = ref [||] in
  for i = Array.length a - 1 downto 0 do
    r := mul_add_small !r base a.(i);
    let lo = ref 0 and hi = ref (base - 1) in
    while !lo < !hi do
      let mid = (!lo + !hi + 1) / 2 in
      if cmp_mag (mul_add_small b mid 0) !r <= 0 then lo := mid
      else hi := mid - 1
    done;
    q.(i) <- !lo;
    if !lo > 0 then r := (make false (sub_mag !r (mul_add_small b !lo 0))).mag
  done;
  ((make false q).mag, !r)

(* ---------- arithmetic ---------- *)

let compare a b =
  match (a.neg, b.neg) with
  | false, true -> 1
  | true, false -> -1
  | false, false -> cmp_mag a.mag b.mag
  | true, true -> cmp_mag b.mag a.mag

let equal a b = compare a b = 0
let neg a = make (not a.neg) a.mag

let add a b =
  if a.neg = b.neg then make a.neg (add_mag a.mag b.mag)
  else if cmp_mag a.mag b.mag >= 0 then make a.neg (sub_mag a.mag b.mag)
  else make b.neg (sub_mag b.mag a.mag)

let sub a b = add a (neg b)
let mul a b = make (a.neg <> b.neg) (mul_mag a.mag b.mag)

(* JS [/] truncates toward zero and [%] takes the sign of the dividend. *)
let div a b =
  if b.mag = [||] then raise Division_by_zero;
  make (a.neg <> b.neg) (fst (divmod_mag a.mag b.mag))

let rem a b =
  if b.mag = [||] then raise Division_by_zero;
  make a.neg (snd (divmod_mag a.mag b.mag))

(* ---------- conversions ---------- *)

let to_string a =
  let n = Array.length a.mag in
  if n = 0 then "0"
  else
    let b = Buffer.create ((n * 4) + 1) in
    if a.neg then Buffer.add_char b '-';
    Buffer.add_string b (string_of_int a.mag.(n - 1));
    for i = n - 2 downto 0 do
      Buffer.add_string b (Printf.sprintf "%04d" a.mag.(i))
    done;
    Buffer.contents b

let to_int_opt a =
  if compare a (of_int min_int) < 0 || compare a (of_int max_int) > 0 then None
  else
    (* Accumulate on the negative side so [min_int] fits. *)
    let acc = ref 0 in
    for i = Array.length a.mag - 1 downto 0 do
      acc := (!acc * base) - a.mag.(i)
    done;
    Some (if a.neg then !acc else - !acc)

let of_bool b = if b then one else zero

(* BigInt(number): integral numbers convert exactly, even above 2^53. *)
let of_float x =
  if not (Float.is_integer x) then
    Core.error
      "The number %s cannot be converted to a BigInt because it is not an \
       integer"
      (Js.number_to_string x)
  else if Float.abs x < 0x1p62 then of_int (int_of_float x)
  else
    let m, e = Float.frexp (Float.abs x) in
    let mag = ref (of_int (int_of_float (Float.ldexp m 53))).mag in
    for _ = 1 to e - 53 do
      mag := mul_add_small !mag 2 0
    done;
    make (x < 0.0) !mag

(* StrWhiteSpaceChar: WhiteSpace and LineTerminator. *)
let is_js_space cp =
  match cp with
  | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x20 | 0xA0 | 0x1680 | 0x2028 | 0x2029
  | 0x202F | 0x205F | 0x3000 | 0xFEFF ->
      true
  | _ -> cp >= 0x2000 && cp <= 0x200A

let js_trim s =
  let rec spans i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      spans (i + n) ((i, n, Uchar.to_int (Uchar.utf_decode_uchar d)) :: acc)
  in
  let content = List.filter (fun (_, _, cp) -> not (is_js_space cp)) in
  match content (spans 0 []) with
  | [] -> ""
  | (start, _, _) :: _ as l ->
      let last, n, _ = List.nth l (List.length l - 1) in
      String.sub s start (last + n - start)

(* BigInt(string), i.e. StringToBigInt: surrounding whitespace is ignored, ""
   is 0, a sign is only allowed on decimals, and 0x/0o/0b select a radix. *)
let of_string s =
  let fail () = Core.error "Cannot convert %s to a BigInt" s in
  let t = js_trim s in
  let len = String.length t in
  let radix, neg, start =
    if len >= 2 && t.[0] = '0' then
      match t.[1] with
      | 'x' | 'X' -> (16, false, 2)
      | 'o' | 'O' -> (8, false, 2)
      | 'b' | 'B' -> (2, false, 2)
      | _ -> (10, false, 0)
    else if len >= 1 && t.[0] = '-' then (10, true, 1)
    else if len >= 1 && t.[0] = '+' then (10, false, 1)
    else (10, false, 0)
  in
  if len = 0 then zero
  else if start = len then fail ()
  else
    let digit c =
      let d =
        match c with
        | '0' .. '9' -> Char.code c - 48
        | 'a' .. 'z' -> Char.code c - 87
        | 'A' .. 'Z' -> Char.code c - 55
        | _ -> radix
      in
      if d >= radix then fail () else d
    in
    let mag = ref [||] in
    for i = start to len - 1 do
      mag := mul_add_small !mag radix (digit t.[i])
    done;
    make neg !mag
