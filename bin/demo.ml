(* Prints one value per module. Compare with faker-js:
     dune exec ./bin/demo.exe -- 42
     node -e "const {faker}=require('@faker-js/faker');faker.seed(42);..." *)

let () =
  let seed = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 42 in
  let f = Faker.create ~seed () in
  Faker.set_default_ref_date f (Faker.Date_util.of_iso "2025-01-01T00:00:00.000Z");
  let show name v = Printf.printf "%-28s %s\n" name v in
  show "person.fullName" (Faker.Person.full_name f);
  show "person.jobTitle" (Faker.Person.job_title f);
  show "location.streetAddress" (Faker.Location.street_address ~use_full_address:true f);
  show "location.city" (Faker.Location.city f);
  show "phone.number" (Faker.Phone.number f);
  show "company.name" (Faker.Company.name f);
  show "company.catchPhrase" (Faker.Company.catch_phrase f);
  show "commerce.productName" (Faker.Commerce.product_name f);
  show "commerce.price" (Faker.Commerce.price f);
  show "finance.iban" (Faker.Finance.iban ~formatted:true f);
  show "finance.creditCardNumber" (Faker.Finance.credit_card_number f);
  show "finance.amount" (Faker.Finance.amount f);
  show "color.human" (Faker.Color.human f);
  show "color.rgb" (Faker.Color.rgb f);
  show "date.past" (Faker.Date_util.to_iso (Faker.Date.past f));
  show "date.birthdate" (Faker.Date_util.to_iso (Faker.Date.birthdate f));
  show "date.month" (Faker.Date.month f);
  show "lorem.sentence" (Faker.Lorem.sentence f);
  show "word.noun" (Faker.Word.noun f);
  show "hacker.phrase" (Faker.Hacker.phrase f);
  show "database.mongodbObjectId" (Faker.Database.mongodb_object_id f);
  show "airline.flightNumber" (Faker.Airline.flight_number f);
  show "animal.dog" (Faker.Animal.dog f);
  show "book.title" (Faker.Book.title f);
  show "food.dish" (Faker.Food.dish f);
  show "music.songName" (Faker.Music.song_name f);
  show "science.unit" (Faker.Science.unit f).name;
  show "internet.email" (Faker.Internet.email f);
  show "internet.url" (Faker.Internet.url f);
  show "internet.ipv4" (Faker.Internet.ipv4 f);
  show "system.filePath" (Faker.System.file_path f);
  show "git.commitSha" (Faker.Git.commit_sha f);
  show "image.avatar" (Faker.Image.avatar f);
  show "vehicle.vehicle" (Faker.Vehicle.vehicle f);
  show "string.uuid" (Faker.String.uuid f);
  show "number.int" (string_of_int (Faker.Number.int ~max:100 f));
  show "helpers.fake" (Faker.Helpers.fake "{{person.firstName}} likes {{food.fruit}}" f);
  show "helpers.fromRegExp" (Faker.Helpers.from_reg_exp "[A-Z]{3}-[0-9]{4}" f)
