(* Port of src/modules/book/module.ts. *)

let pick entry f = Fk_helpers.array_element (Locale.strings f "book" entry) f
let author f = pick "author" f
let format f = pick "format" f
let genre f = pick "genre" f
let publisher f = pick "publisher" f
let series f = pick "series" f
let title f = pick "title" f

let registry : (string * Registry.fn) list =
  [
    ("author", fun f _ -> Json.Str (author f));
    ("format", fun f _ -> Json.Str (format f));
    ("genre", fun f _ -> Json.Str (genre f));
    ("publisher", fun f _ -> Json.Str (publisher f));
    ("series", fun f _ -> Json.Str (series f));
    ("title", fun f _ -> Json.Str (title f));
  ]
