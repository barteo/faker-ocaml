(* faker for OCaml: a port of @faker-js/faker 10.6.0. *)

module Json = Json
module Js = Js

(** JS string semantics: [String.prototype.normalize('NFKD')], [toUpperCase], [toLowerCase] and
    UTF-16 lengths on UTF-8 strings. *)
module Unicode = struct
  include Unicode

  let nfkd = Fk_internet_nfkd.nfkd
end

module Randomizer = Randomizer
module Mersenne = Mersenne
module Distributor = Distributor
module Date_util = Date_util
module Types = Types

exception Faker_error = Core.Faker_error

type t = Core.t
type range = Types.range
type casing = Types.casing
type sex = Types.sex

(* ---------- modules ---------- *)

module Number = struct
  include Fk_number

  let big_int = Fk_number_bigint.big_int
end

module Datatype = Fk_datatype
module String = Fk_string

module Helpers = struct
  include Fk_helpers

  let from_reg_exp = Fk_helpers_regexp.from_reg_exp
  let fake = Fake.fake
  let fake_one_of = Fake.fake_one_of
  let mustache = Fake.mustache
end

module Airline = Fk_airline
module Animal = Fk_animal
module Book = Fk_book
module Color = Fk_color
module Commerce = Fk_commerce
module Company = Fk_company
module Database = Fk_database
module Date = Fk_date
module Finance = Fk_finance
module Food = Fk_food
module Git = Fk_git
module Hacker = Fk_hacker
module Image = Fk_image
module Internet = Fk_internet
module Location = Fk_location
module Lorem = Fk_lorem
module Music = Fk_music
module Person = Fk_person
module Phone = Fk_phone
module Science = Fk_science
module System = Fk_system
module Vehicle = Fk_vehicle
module Word = Fk_word

(* ---------- helpers.fake() registry ---------- *)

let () =
  Registry.add "number" Fk_number.registry;
  Registry.add "number"
    [
      ( "bigInt",
        fun f a ->
          let o = Args.opts ~shorthand:"max" a in
          Json.Str
            (string_of_int
               (Fk_number_bigint.big_int ?min:(Args.int o "min") ?max:(Args.int o "max")
                  ?multiple_of:(Args.int o "multipleOf") f)) );
    ];
  Registry.add "datatype" Fk_datatype.registry;
  Registry.add "string" Fk_string.registry;
  Registry.add "helpers" Fk_helpers_registry.registry;
  Registry.add "airline" Fk_airline.registry;
  Registry.add "animal" Fk_animal.registry;
  Registry.add "book" Fk_book.registry;
  Registry.add "color" Fk_color.registry;
  Registry.add "commerce" Fk_commerce.registry;
  Registry.add "company" Fk_company.registry;
  Registry.add "database" Fk_database.registry;
  Registry.add "date" Fk_date.registry;
  Registry.add "finance" Fk_finance.registry;
  Registry.add "food" Fk_food.registry;
  Registry.add "git" Fk_git.registry;
  Registry.add "hacker" Fk_hacker.registry;
  Registry.add "image" Fk_image.registry;
  Registry.add "internet" Fk_internet.registry;
  Registry.add "location" Fk_location.registry;
  Registry.add "lorem" Fk_lorem.registry;
  Registry.add "music" Fk_music.registry;
  Registry.add "person" Fk_person.registry;
  Registry.add "phone" Fk_phone.registry;
  Registry.add "science" Fk_science.registry;
  Registry.add "system" Fk_system.registry;
  Registry.add "vehicle" Fk_vehicle.registry;
  Registry.add "word" Fk_word.registry

(* ---------- instances ---------- *)

(** [create ?locale ?randomizer ?seed ?default_ref_date ()] creates a new faker instance.
    [locale] defaults to [[en; base]]; an empty list raises like faker-js. [default_ref_date]
    (epoch milliseconds) is faker-js [config.defaultRefDate]. *)
let create ?(locale = [ Lazy.force Locale.en; Lazy.force Locale.base ]) ?randomizer ?seed
    ?default_ref_date () =
  if locale = [] then Core.error "The locale option must contain at least one locale definition.";
  let f = Core.create ~locale ?randomizer ?seed () in
  Option.iter (fun ms -> f.default_ref_date <- (fun () -> ms)) default_ref_date;
  f

let seed (f : t) s = f.randomizer.seed (`Int s)
let seed_array (f : t) a = f.randomizer.seed (`Array a)

(** [seed_random f] reseeds [f] with a random seed and returns it, like faker-js [seed()]. *)
let seed_random (f : t) =
  let s = Randomizer.random_seed () in
  seed f s;
  s

(** Sets a fixed reference date (epoch milliseconds) used by date-relative methods. *)
let set_default_ref_date (f : t) ms = f.default_ref_date <- (fun () -> ms)

(** [set_default_ref_date_input f d] is faker-js [setDefaultRefDate(d)] for a date string or
    number, which is converted ([new Date(d)]) each time it is read. *)
let set_default_ref_date_input (f : t) (d : Fk_date.input) =
  f.default_ref_date <- (fun () -> Fk_date.new_date d)

(** Makes date-relative methods use the current time again, like faker-js
    [setDefaultRefDate()]. *)
let reset_default_ref_date (f : t) = f.default_ref_date <- Core.now

let set_default_ref_date_source (f : t) src = f.default_ref_date <- src
let default_ref_date (f : t) = f.default_ref_date ()

(** Every faker-js locale: [Locales.De.faker ()] is faker-js [fakerDE], [Locales.De.definition ()]
    is [de] and [Locales.De.chain ()] is its fallback chain ([de; en; base]). A program links only
    the locales it references. *)
module Locales = Faker_locales

(** [allLocales] and [allFakers]. Referencing this module links every locale. *)
module All_locales = Faker_all_locales

(** The raw merged locale definitions, like faker-js [rawDefinitions]. *)
let definitions (f : t) = f.locale

(** [definition f category entry] is faker-js [definitions.<category>.<entry>]. It raises the
    upstream "missing" or "not applicable" error when the locale has no such data. *)
let definition (f : t) category entry = Locale.get f category entry

type metadata = {
  title : string option;
  code : string option;
  country : string option;
  language : string option;
  endonym : string option;
  dir : [ `Ltr | `Rtl ] option;
  script : string option;
  variant : string option;
}

(** The locale's metadata, like faker-js [getMetadata()]. Every field is absent for a locale
    without metadata. *)
let get_metadata (f : t) : metadata =
  let field k =
    match Option.bind (Json.member "metadata" f.locale) (Json.member k) with
    | Some (Json.Str s) -> Some s
    | _ -> None
  in
  {
    title = field "title";
    code = field "code";
    country = field "country";
    language = field "language";
    endonym = field "endonym";
    dir = (match field "dir" with Some "rtl" -> Some `Rtl | Some "ltr" -> Some `Ltr | _ -> None);
    script = field "script";
    variant = field "variant";
  }

(** [merge_locales locales] merges locale definitions, earlier ones taking precedence per entry,
    like faker-js [mergeLocales]. *)
let merge_locales = Core.merge_locales

(** The [{{module.method}}] registry behind [Helpers.fake]: upstream (camelCase) method names
    mapped to functions taking JSON arguments. *)
module Registry = Registry

(** The shared default instance (random seed), like faker-js [faker]. *)
let default = lazy (create ())

(** [create_simple ?randomizer ?seed ?default_ref_date ()] is faker-js [new SimpleFaker()]: an
    instance without locale data, for the locale-independent methods (Number, String, Datatype,
    most of Date, Helpers). Methods that need locale data raise the upstream "missing" error. *)
let create_simple ?randomizer ?seed ?default_ref_date () =
  create ~locale:[ Json.Obj [] ] ?randomizer ?seed ?default_ref_date ()

(** The shared [SimpleFaker] instance (random seed), like faker-js [simpleFaker]. *)
let simple_faker = lazy (create_simple ())
