(* Port of src/modules/company/module.ts. *)

let el entry f = Fk_helpers.array_element (Locale.strings f "company" entry) f
let name f = Fake.fake_json (Locale.get f "company" "name_pattern") f
let catch_phrase_adjective f = el "adjective" f
let catch_phrase_descriptor f = el "descriptor" f
let catch_phrase_noun f = el "noun" f
let buzz_adjective f = el "buzz_adjective" f
let buzz_verb f = el "buzz_verb" f
let buzz_noun f = el "buzz_noun" f

let catch_phrase f =
  let a = catch_phrase_adjective f in
  let d = catch_phrase_descriptor f in
  let n = catch_phrase_noun f in
  String.concat " " [ a; d; n ]

let buzz_phrase f =
  let v = buzz_verb f in
  let a = buzz_adjective f in
  let n = buzz_noun f in
  String.concat " " [ v; a; n ]

let registry : (string * Registry.fn) list =
  let s g = fun f _ -> Args.str (g f) in
  [
    ("name", s name);
    ("catchPhrase", s catch_phrase);
    ("buzzPhrase", s buzz_phrase);
    ("catchPhraseAdjective", s catch_phrase_adjective);
    ("catchPhraseDescriptor", s catch_phrase_descriptor);
    ("catchPhraseNoun", s catch_phrase_noun);
    ("buzzAdjective", s buzz_adjective);
    ("buzzVerb", s buzz_verb);
    ("buzzNoun", s buzz_noun);
  ]
