(* Port of src/modules/food/module.ts. *)

let pick entry f = Fk_helpers.array_element (Locale.strings f "food" entry) f

(* text.split(' ').map((word) => word.charAt(0).toUpperCase() + word.slice(1)).join(' ').
   The en food data (and the person names used by its patterns) only start words with ASCII
   characters, so an ASCII capitalization of the first byte is equivalent. *)
let to_title_case text =
  String.concat " " (List.map String.capitalize_ascii (String.split_on_char ' ' text))

let adjective f = pick "adjective" f
let description f = Fake.fake_json (Locale.get f "food" "description_pattern") f

let dish f =
  (* A 50/50 mix of specific dishes and dish_patterns *)
  if Fk_datatype.boolean f then to_title_case (Fake.fake_json (Locale.get f "food" "dish_pattern") f)
  else to_title_case (pick "dish" f)

let ethnic_category f = pick "ethnic_category" f
let fruit f = pick "fruit" f
let ingredient f = pick "ingredient" f
let meat f = pick "meat" f
let spice f = pick "spice" f
let vegetable f = pick "vegetable" f

let registry : (string * Registry.fn) list =
  [
    ("adjective", fun f _ -> Json.Str (adjective f));
    ("description", fun f _ -> Json.Str (description f));
    ("dish", fun f _ -> Json.Str (dish f));
    ("ethnicCategory", fun f _ -> Json.Str (ethnic_category f));
    ("fruit", fun f _ -> Json.Str (fruit f));
    ("ingredient", fun f _ -> Json.Str (ingredient f));
    ("meat", fun f _ -> Json.Str (meat f));
    ("spice", fun f _ -> Json.Str (spice f));
    ("vegetable", fun f _ -> Json.Str (vegetable f));
  ]
