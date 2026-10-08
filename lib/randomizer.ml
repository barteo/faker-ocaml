(* Port of faker-js src/randomizer.ts and src/utils/mersenne.ts. *)

type seed = [ `Int of int | `Array of int array ]

type t = {
  next : unit -> float;  (** A float in [0, 1). *)
  seed : seed -> unit;
}

(* Port of src/internal/seed.ts: [Math.ceil(Math.random() * MAX_SAFE_INTEGER)]. *)
let random_seed =
  let state = lazy (Random.State.make_self_init ()) in
  fun () -> 1 + Random.State.full_int (Lazy.force state) (1 lsl 53 - 1)

let of_mersenne next ?(seed = random_seed ()) () =
  let twister = Mersenne.create (`Int seed) in
  { next = (fun () -> next twister); seed = (fun s -> Mersenne.seed twister s) }

(** The default randomizer since faker v9 (53 bits of precision). *)
let mersenne53 ?seed () = of_mersenne Mersenne.next_f53 ?seed ()

(** The default randomizer prior to faker v9 (32 bits of precision). *)
let mersenne32 ?seed () = of_mersenne Mersenne.next_f32 ?seed ()
