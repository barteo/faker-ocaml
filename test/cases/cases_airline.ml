open T
module A = Faker.Airline

let cases : case list =
  [
    ("airport", fun f -> A.airport_to_json (A.airport f));
    ("airline", fun f -> A.airline_to_json (A.airline f));
    ("airplane", fun f -> A.airplane_to_json (A.airplane f));
    ("recordLocator", fun f -> s (A.record_locator f));
    ("recordLocator/numerics", fun f -> s (A.record_locator ~allow_numerics:true f));
    ("recordLocator/similar", fun f -> s (A.record_locator ~allow_visually_similar_characters:true f));
    ( "recordLocator/both",
      fun f -> s (A.record_locator ~allow_numerics:true ~allow_visually_similar_characters:true f) );
    ("seat", fun f -> s (A.seat f));
    ("seat/narrowbody", fun f -> s (A.seat ~aircraft_type:`Narrowbody f));
    ("seat/regional", fun f -> s (A.seat ~aircraft_type:`Regional f));
    ("seat/widebody", fun f -> s (A.seat ~aircraft_type:`Widebody f));
    ("aircraftType", fun f -> s (A.aircraft_type_to_string (A.aircraft_type f)));
    ("flightNumber", fun f -> s (A.flight_number f));
    ("flightNumber/len3", fun f -> s (A.flight_number ~length:(`N 3) f));
    ("flightNumber/range", fun f -> s (A.flight_number ~length:(`Range (2, 3)) f));
    ("flightNumber/zeros", fun f -> s (A.flight_number ~add_leading_zeros:true f));
    ("flightNumber/len2zeros", fun f -> s (A.flight_number ~length:(`N 2) ~add_leading_zeros:true f));
    ("flightNumber/len6zeros", fun f -> s (A.flight_number ~length:(`N 6) ~add_leading_zeros:true f));
    ("flightNumber/len0", fun f -> s (A.flight_number ~length:(`N 0) ~add_leading_zeros:true f));
    ("flightNumber/err", fun f -> s (A.flight_number ~length:(`Range (5, 1)) f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{airline.seat({\"aircraftType\":\"widebody\"})}} \
              {{airline.flightNumber({\"length\":3,\"addLeadingZeros\":true})}} \
              {{airline.recordLocator({\"allowNumerics\":true})}} {{airline.aircraftType}} \
              {{airline.airport.iataCode}} {{airline.airline.name}}"
             f) );
  ]
