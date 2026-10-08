(* Port of src/modules/animal/module.ts. *)

let pick entry f = Fk_helpers.array_element (Locale.strings f "animal" entry) f

let dog f = pick "dog" f
let cat f = pick "cat" f
let snake f = pick "snake" f
let bear f = pick "bear" f
let lion f = pick "lion" f
let cetacean f = pick "cetacean" f
let horse f = pick "horse" f
let bird f = pick "bird" f
let cow f = pick "cow" f
let fish f = pick "fish" f
let crocodilia f = pick "crocodilia" f
let insect f = pick "insect" f
let rabbit f = pick "rabbit" f
let rodent f = pick "rodent" f
let type_ f = pick "type" f
let pet_name f = pick "pet_name" f

let registry : (string * Registry.fn) list =
  [
    ("dog", fun f _ -> Json.Str (dog f));
    ("cat", fun f _ -> Json.Str (cat f));
    ("snake", fun f _ -> Json.Str (snake f));
    ("bear", fun f _ -> Json.Str (bear f));
    ("lion", fun f _ -> Json.Str (lion f));
    ("cetacean", fun f _ -> Json.Str (cetacean f));
    ("horse", fun f _ -> Json.Str (horse f));
    ("bird", fun f _ -> Json.Str (bird f));
    ("cow", fun f _ -> Json.Str (cow f));
    ("fish", fun f _ -> Json.Str (fish f));
    ("crocodilia", fun f _ -> Json.Str (crocodilia f));
    ("insect", fun f _ -> Json.Str (insect f));
    ("rabbit", fun f _ -> Json.Str (rabbit f));
    ("rodent", fun f _ -> Json.Str (rodent f));
    ("type", fun f _ -> Json.Str (type_ f));
    ("petName", fun f _ -> Json.Str (pet_name f));
  ]
