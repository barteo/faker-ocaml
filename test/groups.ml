(* One entry per module group: (name, locale, expected fixtures, OCaml cases). *)
let groups =
  [
    ( "unicode",
      Expected_unicode.locale,
      Expected_unicode.cases,
      Cases_unicode.cases );
    ("core", Expected_core.locale, Expected_core.cases, Cases_core.cases);
    ("number", Expected_number.locale, Expected_number.cases, Cases_number.cases);
    ( "datatype",
      Expected_datatype.locale,
      Expected_datatype.cases,
      Cases_datatype.cases );
    ("string", Expected_string.locale, Expected_string.cases, Cases_string.cases);
    ( "helpers",
      Expected_helpers.locale,
      Expected_helpers.cases,
      Cases_helpers.cases );
    ( "airline",
      Expected_airline.locale,
      Expected_airline.cases,
      Cases_airline.cases );
    ("animal", Expected_animal.locale, Expected_animal.cases, Cases_animal.cases);
    ("book", Expected_book.locale, Expected_book.cases, Cases_book.cases);
    ("color", Expected_color.locale, Expected_color.cases, Cases_color.cases);
    ( "commerce",
      Expected_commerce.locale,
      Expected_commerce.cases,
      Cases_commerce.cases );
    ( "company",
      Expected_company.locale,
      Expected_company.cases,
      Cases_company.cases );
    ( "database",
      Expected_database.locale,
      Expected_database.cases,
      Cases_database.cases );
    ("date", Expected_date.locale, Expected_date.cases, Cases_date.cases);
    ( "finance",
      Expected_finance.locale,
      Expected_finance.cases,
      Cases_finance.cases );
    ("food", Expected_food.locale, Expected_food.cases, Cases_food.cases);
    ("git", Expected_git.locale, Expected_git.cases, Cases_git.cases);
    ("hacker", Expected_hacker.locale, Expected_hacker.cases, Cases_hacker.cases);
    ("image", Expected_image.locale, Expected_image.cases, Cases_image.cases);
    ( "internet",
      Expected_internet.locale,
      Expected_internet.cases,
      Cases_internet.cases );
    ( "location",
      Expected_location.locale,
      Expected_location.cases,
      Cases_location.cases );
    ("lorem", Expected_lorem.locale, Expected_lorem.cases, Cases_lorem.cases);
    ("music", Expected_music.locale, Expected_music.cases, Cases_music.cases);
    ("person", Expected_person.locale, Expected_person.cases, Cases_person.cases);
    ("phone", Expected_phone.locale, Expected_phone.cases, Cases_phone.cases);
    ( "science",
      Expected_science.locale,
      Expected_science.cases,
      Cases_science.cases );
    ("system", Expected_system.locale, Expected_system.cases, Cases_system.cases);
    ( "vehicle",
      Expected_vehicle.locale,
      Expected_vehicle.cases,
      Cases_vehicle.cases );
    ("word", Expected_word.locale, Expected_word.cases, Cases_word.cases);
  ]
