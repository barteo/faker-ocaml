(* faker for OCaml: a port of @faker-js/faker 10.6.0. *)

module Json = Json
module Js = Js
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

(** [create ?locale ?randomizer ?seed ()] creates a new faker instance.
    [locale] defaults to [[en; base]]. *)
let create ?(locale = [ Lazy.force Locale.en; Lazy.force Locale.base ]) ?randomizer ?seed () =
  Core.create ~locale ?randomizer ?seed ()

let seed (f : t) s = f.randomizer.seed (`Int s)
let seed_array (f : t) a = f.randomizer.seed (`Array a)

(** Sets a fixed reference date (epoch milliseconds) used by date-relative methods. *)
let set_default_ref_date (f : t) ms = f.default_ref_date <- (fun () -> ms)

let set_default_ref_date_source (f : t) src = f.default_ref_date <- src
let default_ref_date (f : t) = f.default_ref_date ()

module Locales = struct
  let en () = Lazy.force Locale.en
  let base () = Lazy.force Locale.base
end

(** The raw merged locale definitions. *)
let definitions (f : t) = f.locale

(** The shared default instance (random seed). *)
let default = lazy (create ())
