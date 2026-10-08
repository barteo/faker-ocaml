open T
module M = Faker.Animal

let cases : case list =
  [
    ("dog", fun f -> s (M.dog f));
    ("cat", fun f -> s (M.cat f));
    ("snake", fun f -> s (M.snake f));
    ("bear", fun f -> s (M.bear f));
    ("lion", fun f -> s (M.lion f));
    ("cetacean", fun f -> s (M.cetacean f));
    ("horse", fun f -> s (M.horse f));
    ("bird", fun f -> s (M.bird f));
    ("cow", fun f -> s (M.cow f));
    ("fish", fun f -> s (M.fish f));
    ("crocodilia", fun f -> s (M.crocodilia f));
    ("insect", fun f -> s (M.insect f));
    ("rabbit", fun f -> s (M.rabbit f));
    ("rodent", fun f -> s (M.rodent f));
    ("type", fun f -> s (M.type_ f));
    ("petName", fun f -> s (M.pet_name f));
    ("fake", fun f -> s (Faker.Helpers.fake "{{animal.dog}}|{{animal.cat}}|{{animal.snake}}|{{animal.bear}}|{{animal.lion}}|{{animal.cetacean}}|{{animal.horse}}|{{animal.bird}}|{{animal.cow}}|{{animal.fish}}|{{animal.crocodilia}}|{{animal.insect}}|{{animal.rabbit}}|{{animal.rodent}}|{{animal.type}}|{{animal.petName}}|" f));
  ]
