(* Port of src/modules/database/module.ts. *)

let pick entry f =
  Fk_helpers.array_element (Locale.strings f "database" entry) f

let column f = pick "column" f
let type_ f = pick "type" f
let collation f = pick "collation" f
let engine f = pick "engine" f

let mongodb_object_id f =
  Fk_string.hexadecimal ~length:(`N 24) ~casing:`Lower ~prefix:"" f

let registry : (string * Registry.fn) list =
  [
    ("column", fun f _ -> Args.str (column f));
    ("type", fun f _ -> Args.str (type_ f));
    ("collation", fun f _ -> Args.str (collation f));
    ("engine", fun f _ -> Args.str (engine f));
    ("mongodbObjectId", fun f _ -> Args.str (mongodb_object_id f));
  ]
