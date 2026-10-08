open T
module M = Faker.Book

let cases : case list =
  [
    ("author", fun f -> s (M.author f));
    ("format", fun f -> s (M.format f));
    ("genre", fun f -> s (M.genre f));
    ("publisher", fun f -> s (M.publisher f));
    ("series", fun f -> s (M.series f));
    ("title", fun f -> s (M.title f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{book.author}}|{{book.format}}|{{book.genre}}|{{book.publisher}}|{{book.series}}|{{book.title}}|"
             f) );
  ]
