(* Port of faker-js src/internal/mersenne.ts (MersenneTwister19937).

   All state words are kept as unsigned 32-bit values inside native ints.
   JavaScript performs the same operations on signed int32 / float values,
   but every value is only ever observed modulo 2^32, so the results match. *)

let n = 624
let m = 397
let a = 0x9908b0df
let f = 1812433253
let u = 11
let s = 7
let b = 0x9d2c5680
let t = 15
let c = 0xefc60000
let l = 18
let mask32 = 0xFFFFFFFF
let mask_lower = 0x7FFFFFFF
let mask_upper = 0x80000000
let high_multiplier_53 = 67108864
let floatify_53 = 1.0 /. 9007199254740992.0
let floatify_32 = 1.0 /. 4294967296.0

(* Low 32 bits of the product; correct even when the native product overflows
   because 2^63 is a multiple of 2^32. *)
let imul x y = x * y land mask32
let u32 x = x land mask32

let number_seeded seed =
  let out = Array.make n 0 in
  out.(0) <- u32 seed;
  for idx = 1 to n - 1 do
    let prev = out.(idx - 1) in
    let xored = prev lxor (prev lsr 30) in
    out.(idx) <- u32 (imul f xored + idx)
  done;
  out

let array_seeded (seed : int array) =
  let out = number_seeded 19650218 in
  let len = Array.length seed in
  let idx_out = ref 1 in
  let idx_seed = ref 0 in
  for _ = max n len downto 1 do
    let prev = out.(!idx_out - 1) in
    let xored = prev lxor (prev lsr 30) in
    out.(!idx_out) <-
      u32
        ((out.(!idx_out) lxor imul xored 1664525) + seed.(!idx_seed) + !idx_seed);
    incr idx_out;
    incr idx_seed;
    if !idx_out >= n then begin
      out.(0) <- out.(n - 1);
      idx_out := 1
    end;
    if !idx_seed >= len then idx_seed := 0
  done;
  for _ = n - 1 downto 1 do
    let prev = out.(!idx_out - 1) in
    out.(!idx_out) <-
      u32
        ((out.(!idx_out) lxor imul (prev lxor (prev lsr 30)) 1566083941)
        - !idx_out);
    incr idx_out;
    if !idx_out >= n then begin
      out.(0) <- out.(n - 1);
      idx_out := 1
    end
  done;
  out.(0) <- 0x80000000;
  out

let twist states =
  let mix i j k =
    let y = (states.(i) land mask_upper) + (states.(j) land mask_lower) in
    states.(i) <- (states.(k) lxor (y lsr 1) lxor if y land 1 = 1 then a else 0)
  in
  for idx = 0 to n - m - 1 do
    mix idx (idx + 1) (idx + m)
  done;
  for idx = n - m to n - 2 do
    mix idx (idx + 1) (idx + m - n)
  done;
  mix (n - 1) 0 (m - 1);
  states

type t = { mutable states : int array; mutable index : int }

let seed_from = function
  | `Int seed -> number_seeded seed
  | `Array seed -> array_seeded seed

let create seed = { states = twist (seed_from seed); index = 0 }

let seed g seed =
  g.states <- twist (seed_from seed);
  g.index <- 0

let next_u32 g =
  let y = g.states.(g.index) in
  let y = y lxor (y lsr u) in
  let y = y lxor ((y lsl s) land b) in
  let y = y lxor ((y lsl t) land c) in
  let y = y lxor (y lsr l) in
  g.index <- g.index + 1;
  if g.index >= n then begin
    g.states <- twist g.states;
    g.index <- 0
  end;
  u32 y

let next_f32 g = float_of_int (next_u32 g) *. floatify_32

let next_u53 g =
  let high = next_u32 g lsr 5 in
  let low = next_u32 g lsr 6 in
  (high * high_multiplier_53) + low

let next_f53 g = float_of_int (next_u53 g) *. floatify_53
