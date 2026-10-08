(* Port of src/modules/hacker/module.ts. *)

let pick entry f = Fk_helpers.array_element (Locale.strings f "hacker" entry) f
let abbreviation f = pick "abbreviation" f
let adjective f = pick "adjective" f
let noun f = pick "noun" f
let verb f = pick "verb" f
let ingverb f = pick "ingverb" f
let phrase f = Fake.fake_json (Locale.get f "hacker" "phrase") f

let registry : (string * Registry.fn) list =
  [
    ("abbreviation", fun f _ -> Args.str (abbreviation f));
    ("adjective", fun f _ -> Args.str (adjective f));
    ("noun", fun f _ -> Args.str (noun f));
    ("verb", fun f _ -> Args.str (verb f));
    ("ingverb", fun f _ -> Args.str (ingverb f));
    ("phrase", fun f _ -> Args.str (phrase f));
  ]
