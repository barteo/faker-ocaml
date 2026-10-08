open T
module C = Faker.Company

let cases : case list =
  [
    ("name", fun f -> s (C.name f));
    ("catchPhrase", fun f -> s (C.catch_phrase f));
    ("buzzPhrase", fun f -> s (C.buzz_phrase f));
    ("catchPhraseAdjective", fun f -> s (C.catch_phrase_adjective f));
    ("catchPhraseDescriptor", fun f -> s (C.catch_phrase_descriptor f));
    ("catchPhraseNoun", fun f -> s (C.catch_phrase_noun f));
    ("buzzAdjective", fun f -> s (C.buzz_adjective f));
    ("buzzVerb", fun f -> s (C.buzz_verb f));
    ("buzzNoun", fun f -> s (C.buzz_noun f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{company.name}}|{{company.catchPhrase}}|{{company.buzzPhrase}}|{{company.catchPhraseAdjective}}|{{company.catchPhraseDescriptor}}|{{company.catchPhraseNoun}}|{{company.buzzAdjective}}|{{company.buzzVerb}}|{{company.buzzNoun}}"
             f) );
  ]
