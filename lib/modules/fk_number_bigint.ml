(* number.bigInt (src/modules/number/module.ts). JavaScript BigInt is mapped
   to native int (63-bit), so ranges wider than ~4.6e17 are rejected. *)

let big_int ?(min = 0) ?max ?(multiple_of = 1) f =
  let max = match max with Some m -> m | None -> min + 999999999999999 in
  if max < min then Core.error "Max %d should be larger than min %d." max min;
  if multiple_of <= 0 then Core.error "multipleOf should be greater than 0.";
  let effective_min =
    (min / multiple_of) + if min mod multiple_of > 0 then 1 else 0
  in
  let effective_max =
    (max / multiple_of) - if max mod multiple_of < 0 then 1 else 0
  in
  if effective_min = effective_max then effective_min * multiple_of
  else if effective_max < effective_min then
    Core.error "No suitable bigint value between %d and %d found." min max
  else begin
    let delta = effective_max - effective_min + 1 in
    if delta > 461168601842738790 then
      Core.error
        "bigInt ranges wider than 4.6e17 are not supported by the OCaml port.";
    let digits =
      Fk_string.numeric
        ~length:(`N (String.length (string_of_int delta)))
        ~allow_leading_zeros:true f
    in
    let offset = ref 0 in
    String.iter
      (fun c -> offset := ((!offset * 10) + (Char.code c - 48)) mod delta)
      digits;
    (effective_min + !offset) * multiple_of
  end
