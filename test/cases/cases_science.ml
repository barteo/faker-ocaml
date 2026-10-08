open T
module M = Faker.Science

let cases : case list =
  [
    ("chemicalElement", fun f -> M.chemical_element_to_json (M.chemical_element f));
    ("unit", fun f -> M.unit_to_json (M.unit f));
    ("unit/many", fun f -> arr M.unit_to_json (Faker.Helpers.multiple ~count:(`N 20) (fun _ -> M.unit f) f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{science.chemicalElement.name}} {{science.chemicalElement.atomicNumber}} \
              {{science.unit.symbol}} {{science.chemicalElement}}"
             f) );
  ]
