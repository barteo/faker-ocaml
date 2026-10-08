(* Port of src/modules/datatype/module.ts. *)

let boolean ?(probability = 0.5) f =
  if probability <= 0.0 then false
  else if probability >= 1.0 then true
  else Fk_number.float f < probability

let registry : (string * Registry.fn) list =
  [
    ( "boolean",
      fun f a ->
        let o = Args.opts ~shorthand:"probability" a in
        Args.bool_ (boolean ?probability:(Args.float o "probability") f) );
  ]
