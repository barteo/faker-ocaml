(* number.bigInt (src/modules/number/module.ts). JavaScript BigInt is mapped
   to [Bigint.t], so any range works. *)

(* [multiple_of] is a thunk so the registry converts it at the same point as
   upstream: after the min/max check. *)
let big_int_lazy ?(min = Bigint.zero) ?max ?(multiple_of = fun () -> Bigint.one)
    f =
  let open Bigint in
  let max =
    match max with Some m -> m | None -> add min (of_int 999999999999999)
  in
  if compare max min < 0 then
    Core.error "Max %s should be larger than min %s." (to_string max)
      (to_string min);
  let multiple_of = multiple_of () in
  if compare multiple_of zero <= 0 then
    Core.error "multipleOf should be greater than 0.";
  (* Math.ceil(min / multipleOf) *)
  let effective_min =
    add (div min multiple_of)
      (if compare (rem min multiple_of) zero > 0 then one else zero)
  in
  (* Math.floor(max / multipleOf) *)
  let effective_max =
    sub (div max multiple_of)
      (if compare (rem max multiple_of) zero < 0 then one else zero)
  in
  if equal effective_min effective_max then mul effective_min multiple_of
  else if compare effective_max effective_min < 0 then
    Core.error "No suitable bigint value between %s and %s found."
      (to_string min) (to_string max)
  else
    (* +1 for inclusive max bounds and even distribution *)
    let delta = add (sub effective_max effective_min) one in
    let offset =
      rem
        (of_string
           (Fk_string.numeric
              ~length:(`N (String.length (to_string delta)))
              ~allow_leading_zeros:true f))
        delta
    in
    mul (add effective_min offset) multiple_of

let big_int ?min ?max ?multiple_of f =
  big_int_lazy ?min ?max ?multiple_of:(Option.map (fun m () -> m) multiple_of) f
