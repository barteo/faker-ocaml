open T
module V = Faker.Vehicle

let cases : case list =
  [
    ("vehicle", fun f -> s (V.vehicle f));
    ("manufacturer", fun f -> s (V.manufacturer f));
    ("model", fun f -> s (V.model f));
    ("type", fun f -> s (V.type_ f));
    ("fuel", fun f -> s (V.fuel f));
    ("vin", fun f -> s (V.vin f));
    ("color", fun f -> s (V.color f));
    ("vrm", fun f -> s (V.vrm f));
    ("bicycle", fun f -> s (V.bicycle f));
    ( "vin/many",
      fun f -> ss (Faker.Helpers.multiple ~count:(`N 30) (fun _ -> V.vin f) f)
    );
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{vehicle.vehicle}} | {{vehicle.manufacturer}} | \
              {{vehicle.model}} | {{vehicle.type}} | {{vehicle.fuel}} | \
              {{vehicle.vin}} | {{vehicle.color}} | {{vehicle.vrm}} | \
              {{vehicle.bicycle}}"
             f) );
  ]
