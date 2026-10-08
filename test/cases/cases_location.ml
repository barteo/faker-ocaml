open T
module L = Faker.Location

let pair (a, b) = J.Arr [| n a; n b |]
let fake p f = s (Faker.Helpers.fake p f)

let cases : case list =
  [
    ("latitude", fun f -> n (L.latitude f));
    ("latitude/opts", fun f -> n (L.latitude ~max:10.0 ~min:(-10.0) f));
    ( "latitude/precision",
      fun f -> n (L.latitude ~max:45.5 ~min:12.25 ~precision:7 f) );
    ("latitude/p0", fun f -> n (L.latitude ~precision:0 f));
    ("latitude/err", fun f -> n (L.latitude ~max:(-10.0) ~min:10.0 f));
    ("latitude/errprec", fun f -> n (L.latitude ~precision:(-1) f));
    ("longitude", fun f -> n (L.longitude f));
    ("longitude/opts", fun f -> n (L.longitude ~max:10.0 ~min:(-10.0) f));
    ( "longitude/precision",
      fun f -> n (L.longitude ~max:179.99 ~min:100.1 ~precision:2 f) );
    ("longitude/err", fun f -> n (L.longitude ~max:(-100.0) f));
    ("nearbyGPSCoordinate", fun f -> pair (L.nearby_gps_coordinate f));
    ( "nearbyGPSCoordinate/origin",
      fun f -> pair (L.nearby_gps_coordinate ~origin:(33.0, -170.0) f) );
    ( "nearbyGPSCoordinate/metric",
      fun f ->
        pair
          (L.nearby_gps_coordinate ~origin:(33.0, -170.0) ~radius:1000.0
             ~is_metric:true f) );
    ( "nearbyGPSCoordinate/miles",
      fun f ->
        pair (L.nearby_gps_coordinate ~origin:(-12.5, 45.25) ~radius:500.0 f) );
    ( "nearbyGPSCoordinate/pole",
      fun f ->
        pair
          (L.nearby_gps_coordinate ~origin:(89.9, 179.9) ~radius:2000.0
             ~is_metric:true f) );
    ( "nearbyGPSCoordinate/southpole",
      fun f ->
        pair
          (L.nearby_gps_coordinate ~origin:(-89.95, -179.95) ~radius:3000.0 f)
    );
    ( "nearbyGPSCoordinate/huge",
      fun f ->
        pair
          (L.nearby_gps_coordinate ~origin:(10.0, 20.0) ~radius:30000.0
             ~is_metric:true f) );
    ( "nearbyGPSCoordinate/zero",
      fun f -> pair (L.nearby_gps_coordinate ~origin:(0.0, 0.0) ~radius:0.0 f)
    );
    ("zipCode", fun f -> s (L.zip_code f));
    ("zipCode/format", fun f -> s (L.zip_code ~format:"???-###-**" f));
    ("zipCode/formatopt", fun f -> s (L.zip_code ~format:"####" f));
    ("zipCode/state", fun f -> s (L.zip_code ~state:"CA" f));
    ("city", fun f -> s (L.city f));
    ("buildingNumber", fun f -> s (L.building_number f));
    ("street", fun f -> s (L.street f));
    ("streetAddress", fun f -> s (L.street_address f));
    ( "streetAddress/full",
      fun f -> s (L.street_address ~use_full_address:true f) );
    ( "streetAddress/fullopt",
      fun f -> s (L.street_address ~use_full_address:true f) );
    ( "streetAddress/normal",
      fun f -> s (L.street_address ~use_full_address:false f) );
    ("postalAddress", fun f -> s (L.postal_address f));
    ("secondaryAddress", fun f -> s (L.secondary_address f));
    ("county", fun f -> s (L.county f));
    ("country", fun f -> s (L.country f));
    ("continent", fun f -> s (L.continent f));
    ("countryCode", fun f -> s (L.country_code f));
    ("countryCode/alpha2", fun f -> s (L.country_code ~variant:`Alpha_2 f));
    ("countryCode/alpha3", fun f -> s (L.country_code ~variant:`Alpha_3 f));
    ("countryCode/numeric", fun f -> s (L.country_code ~variant:`Numeric f));
    ("state", fun f -> s (L.state f));
    ("state/abbr", fun f -> s (L.state ~abbreviated:true f));
    ("direction", fun f -> s (L.direction f));
    ("direction/abbr", fun f -> s (L.direction ~abbreviated:true f));
    ("cardinalDirection", fun f -> s (L.cardinal_direction f));
    ( "cardinalDirection/abbr",
      fun f -> s (L.cardinal_direction ~abbreviated:true f) );
    ("ordinalDirection", fun f -> s (L.ordinal_direction f));
    ( "ordinalDirection/abbr",
      fun f -> s (L.ordinal_direction ~abbreviated:true f) );
    ("timeZone", fun f -> s (L.time_zone f));
    ("language", fun f -> L.language_to_json (L.language f));
    ( "fake/misc",
      fake
        "{{location.countryCode(\"alpha-3\")}} {{location.zipCode}} \
         {{location.state({\"abbreviated\":true})}} {{location.direction}} \
         {{location.buildingNumber}}" );
    ( "fake/coords",
      fake
        "{{location.latitude}},{{location.longitude({\"precision\":2})}} / \
         {{location.nearbyGPSCoordinate({\"origin\":[1,2],\"radius\":5})}}" );
    ("fake/language", fake "{{location.language.name}} {{location.language}}");
    ("fake/address", fake "{{location.streetAddress(true)}}");
  ]
