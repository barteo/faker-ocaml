open T
module H = Faker.Hacker

let cases : case list =
  [
    ("abbreviation", fun f -> s (H.abbreviation f));
    ("adjective", fun f -> s (H.adjective f));
    ("noun", fun f -> s (H.noun f));
    ("verb", fun f -> s (H.verb f));
    ("ingverb", fun f -> s (H.ingverb f));
    ("phrase", fun f -> s (H.phrase f));
    ("fake", fun f -> s (Faker.Helpers.fake "{{hacker.noun}} {{hacker.phrase}}" f));
  ]
