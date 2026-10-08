(* Math.sin / Math.cos exactly as V8 computes them (used by location.nearbyGPSCoordinate).

   V8 implements them with fdlibm (src/base/ieee754.cc). The reference fixtures come from an
   arm64 build, where clang contracts [a * b + c] within one statement into a fused
   multiply-add (-ffp-contract=on, preferring the left operand), so those contractions are
   spelled out here with [Float.fma] and every other product goes through [Js.mul].
   Verified bit-for-bit against node 25 for all 5-fraction-digit angles in [0, 2pi].
   Arguments with |x| > ~2^19 * pi/2 (which need __kernel_rem_pio2) fall back to libm; they
   cannot occur in this library (angles are within [0, 2pi]). *)

let ( *: ) = Js.mul
let fma = Float.fma

let high x =
  Int64.to_int (Int64.shift_right_logical (Int64.bits_of_float x) 32)
  land 0xffffffff

let kernel_sin x y iy =
  let s1 = -1.66666666666666324348e-01
  and s2 = 8.33333333332248946124e-03
  and s3 = -1.98412698298579493134e-04
  and s4 = 2.75573137070700676789e-06
  and s5 = -2.50507602534068634195e-08
  and s6 = 1.58969099521155010221e-10 in
  let ix = high x land 0x7fffffff in
  if ix < 0x3e400000 then x
  else
    let z = x *: x in
    let v = z *: x in
    let r = fma z (fma z (fma z (fma z s6 s5) s4) s3) s2 in
    if iy = 0 then fma v (fma z r s1) x
    else
      let p = fma 0.5 y (-.(v *: r)) in
      let q = fma z p (-.y) in
      x -. fma (-.v) s1 q

let kernel_cos x y =
  let c1 = 4.16666666666666019037e-02
  and c2 = -1.38888888888741095749e-03
  and c3 = 2.48015872894767294178e-05
  and c4 = -2.75573143513906633035e-07
  and c5 = 2.08757232129817482790e-09
  and c6 = -1.13596475577881948265e-11 in
  let ix = high x land 0x7fffffff in
  if ix < 0x3e400000 then 1.0
  else
    let z = x *: x in
    let r = z *: fma z (fma z (fma z (fma z (fma z c6 c5) c4) c3) c2) c1 in
    if ix < 0x3FD33333 then 1.0 -. fma 0.5 z (-.fma z r (-.(x *: y)))
    else
      let qx =
        if ix > 0x3fe90000 then 0.28125
        else
          Int64.float_of_bits
            (Int64.shift_left (Int64.of_int (ix - 0x00200000)) 32)
      in
      let hz = fma 0.5 z (-.qx) in
      let a = 1.0 -. qx in
      a -. (hz -. fma z r (-.(x *: y)))

let npio2_hw =
  [|
    0x3FF921FB;
    0x400921FB;
    0x4012D97C;
    0x401921FB;
    0x401F6A7A;
    0x4022D97C;
    0x4025FDBB;
    0x402921FB;
    0x402C463A;
    0x402F6A7A;
    0x4031475C;
    0x4032D97C;
    0x40346B9C;
    0x4035FDBB;
    0x40378FDB;
    0x403921FB;
    0x403AB41B;
    0x403C463A;
    0x403DD85A;
    0x403F6A7A;
    0x40407E4C;
    0x4041475C;
    0x4042106C;
    0x4042D97C;
    0x4043A28C;
    0x40446B9C;
    0x404534AC;
    0x4045FDBB;
    0x4046C6CB;
    0x40478FDB;
    0x404858EB;
    0x404921FB;
  |]

(* __ieee754_rem_pio2 for |x| <= 2^19 * pi/2: Some (n, y0, y1) with x = n * pi/2 + y0 + y1. *)
let rem_pio2 x =
  let invpio2 = 6.36619772367581382433e-01
  and pio2_1 = 1.57079632673412561417e+00
  and pio2_1t = 6.07710050650619224932e-11
  and pio2_2 = 6.07710050630396597660e-11
  and pio2_2t = 2.02226624879595063154e-21
  and pio2_3 = 2.02226624871116645580e-21
  and pio2_3t = 8.47842766036889956997e-32 in
  let hx = high x in
  let neg = hx land 0x80000000 <> 0 in
  let ix = hx land 0x7fffffff in
  if ix <= 0x3fe921fb then Some (0, x, 0.0)
  else if ix < 0x4002d97c then
    if not neg then
      let z = x -. pio2_1 in
      if ix <> 0x3ff921fb then
        let y0 = z -. pio2_1t in
        Some (1, y0, z -. y0 -. pio2_1t)
      else
        let z = z -. pio2_2 in
        let y0 = z -. pio2_2t in
        Some (1, y0, z -. y0 -. pio2_2t)
    else
      let z = x +. pio2_1 in
      if ix <> 0x3ff921fb then
        let y0 = z +. pio2_1t in
        Some (-1, y0, z -. y0 +. pio2_1t)
      else
        let z = z +. pio2_2 in
        let y0 = z +. pio2_2t in
        Some (-1, y0, z -. y0 +. pio2_2t)
  else if ix <= 0x413921fb then begin
    let t = Float.abs x in
    let n = Float.to_int (fma t invpio2 0.5) in
    let fn = float_of_int n in
    let r = ref (fma (-.fn) pio2_1 t) in
    let w = ref (fn *: pio2_1t) in
    let y0 =
      if n < 32 && ix <> npio2_hw.(n - 1) then !r -. !w
      else begin
        let j = ix lsr 20 in
        let y0 = ref (!r -. !w) in
        let i = j - ((high !y0 lsr 20) land 0x7ff) in
        if i > 16 then begin
          let t = !r in
          w := fn *: pio2_2;
          r := t -. !w;
          w := fma fn pio2_2t (-.(t -. !r -. !w));
          y0 := !r -. !w;
          let i = j - ((high !y0 lsr 20) land 0x7ff) in
          if i > 49 then begin
            let t = !r in
            w := fn *: pio2_3;
            r := t -. !w;
            w := fma fn pio2_3t (-.(t -. !r -. !w));
            y0 := !r -. !w
          end
        end;
        !y0
      end
    in
    let y1 = !r -. y0 -. !w in
    if neg then Some (-n, -.y0, -.y1) else Some (n, y0, y1)
  end
  else None

let sin x =
  let ix = high x land 0x7fffffff in
  if ix <= 0x3fe921fb then kernel_sin x 0.0 0
  else if ix >= 0x7ff00000 then Float.nan
  else
    match rem_pio2 x with
    | None -> Float.sin x
    | Some (n, y0, y1) -> (
        match n land 3 with
        | 0 -> kernel_sin y0 y1 1
        | 1 -> kernel_cos y0 y1
        | 2 -> -.kernel_sin y0 y1 1
        | _ -> -.kernel_cos y0 y1)

let cos x =
  let ix = high x land 0x7fffffff in
  if ix <= 0x3fe921fb then kernel_cos x 0.0
  else if ix >= 0x7ff00000 then Float.nan
  else
    match rem_pio2 x with
    | None -> Float.cos x
    | Some (n, y0, y1) -> (
        match n land 3 with
        | 0 -> kernel_cos y0 y1
        | 1 -> -.kernel_sin y0 y1 1
        | 2 -> -.kernel_cos y0 y1
        | _ -> kernel_sin y0 y1 1)
