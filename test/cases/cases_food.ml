open T
module M = Faker.Food

let cases : case list =
  [
    ("adjective", fun f -> s (M.adjective f));
    ("description", fun f -> s (M.description f));
    ("dish", fun f -> s (M.dish f));
    ("ethnicCategory", fun f -> s (M.ethnic_category f));
    ("fruit", fun f -> s (M.fruit f));
    ("ingredient", fun f -> s (M.ingredient f));
    ("meat", fun f -> s (M.meat f));
    ("spice", fun f -> s (M.spice f));
    ("vegetable", fun f -> s (M.vegetable f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{food.adjective}}|{{food.description}}|{{food.dish}}|{{food.ethnicCategory}}|{{food.fruit}}|{{food.ingredient}}|{{food.meat}}|{{food.spice}}|{{food.vegetable}}|"
             f) );
    ( "dish/many",
      fun f ->
        arr s (Faker.Helpers.multiple ~count:(`N 40) (fun _ -> M.dish f) f) );
    ( "description/many",
      fun f ->
        arr s
          (Faker.Helpers.multiple ~count:(`N 40) (fun _ -> M.description f) f)
    );
  ]
