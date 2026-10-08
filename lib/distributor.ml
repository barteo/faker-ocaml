(* Port of src/distributors/*.ts. A distributor maps the randomizer to a
   float in [0, 1) with some distribution. *)

type t = Randomizer.t -> float

let uniform : t = fun r -> r.next ()

(** [exponential ?base ?bias ()]: values closer to 0 are more likely when
    [base > 1]. *)
let exponential ?base ?(bias = -1.0) () : t =
  let base =
    match base with
    | Some b -> b
    | None -> if bias <= 0.0 then -.bias +. 1.0 else 1.0 /. (bias +. 1.0)
  in
  if base = 1.0 then uniform
  else if base <= 0.0 then Core.error "Base should be greater than 0."
  else fun r -> (Float.pow base (r.next ()) -. 1.0) /. (base -. 1.0)
