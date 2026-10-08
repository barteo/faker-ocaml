open T
module D = Faker.Datatype

let cases : case list =
  [
    ("boolean", fun f -> b (D.boolean f));
    ("boolean/0.9", fun f -> b (D.boolean ~probability:0.9 f));
    ("boolean/0.1", fun f -> b (D.boolean ~probability:0.1 f));
    ("boolean/1", fun f -> b (D.boolean ~probability:1.0 f));
    ("boolean/0", fun f -> b (D.boolean ~probability:0.0 f));
  ]
