open T
module P = Faker.Person

let sexes : (string * Faker.Person.sex_type option) list =
  [ ("", None); ("/female", Some `Female); ("/male", Some `Male); ("/generic", Some `Generic) ]

let sexed : (string * (?sex:Faker.Person.sex_type -> Faker.t -> string)) list =
  [
    ("firstName", fun ?sex f -> P.first_name ?sex f);
    ("lastName", fun ?sex f -> P.last_name ?sex f);
    ("middleName", fun ?sex f -> P.middle_name ?sex f);
    ("prefix", fun ?sex f -> P.prefix ?sex f);
  ]

let fake p f = s (Faker.Helpers.fake p f)

let cases : case list =
  List.concat_map
    (fun (name, m) -> List.map (fun (suffix, sex) -> (name ^ suffix, fun f -> s (m ?sex f))) sexes)
    sexed
  @ [
      ("fullName", fun f -> s (P.full_name f));
      ("fullName/female", fun f -> s (P.full_name ~sex:`Female f));
      ("fullName/male", fun f -> s (P.full_name ~sex:`Male f));
      ("fullName/generic", fun f -> s (P.full_name ~sex:`Generic f));
      ("fullName/first", fun f -> s (P.full_name ~first_name:"Joann" f));
      ("fullName/last", fun f -> s (P.full_name ~last_name:"Doe" f));
      ("fullName/both", fun f -> s (P.full_name ~first_name:"Jane" ~last_name:"Doe" ~sex:`Female f));
      ("fullName/empty", fun f -> s (P.full_name ~first_name:"" ~last_name:"" f));
      ("fullName/dollar", fun f -> s (P.full_name ~first_name:"$& $1" ~last_name:"$$" f));
      ("gender", fun f -> s (P.gender f));
      ("sex", fun f -> s (P.sex f));
      ("sexType", fun f -> s (P.sex_type_to_string (P.sex_type f)));
      ("sexType/generic", fun f -> s (P.sex_type_to_string (P.sex_type ~include_generic:true f)));
      ("sexType/nogeneric", fun f -> s (P.sex_type_to_string (P.sex_type ~include_generic:false f)));
      ("bio", fun f -> s (P.bio f));
      ("suffix", fun f -> s (P.suffix f));
      ("jobTitle", fun f -> s (P.job_title f));
      ("jobDescriptor", fun f -> s (P.job_descriptor f));
      ("jobArea", fun f -> s (P.job_area f));
      ("jobType", fun f -> s (P.job_type f));
      ("zodiacSign", fun f -> s (P.zodiac_sign f));
      ( "fake/names",
        fake
          {|{{person.firstName}}|{{person.firstName(female)}}|{{person.lastName(male)}}|{{person.middleName(generic)}}|{{person.prefix(male)}}|{{person.fullName}}|{{person.fullName({"sex":"female","firstName":"X"})}}|}
      );
      ( "fake/misc",
        fake
          {|{{person.gender}}|{{person.sex}}|{{person.sexType}}|{{person.sexType({"includeGeneric":true})}}|{{person.suffix}}|{{person.jobTitle}}|{{person.jobDescriptor}}|{{person.jobArea}}|{{person.jobType}}|{{person.zodiacSign}}|{{person.lastName}}|{{person.middleName}}|{{person.prefix}}|}
      );
      ("fake/bio", fake "{{person.bio}}");
    ]
