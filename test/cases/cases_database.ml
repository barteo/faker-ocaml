open T
module D = Faker.Database

let cases : case list =
  [
    ("column", fun f -> s (D.column f));
    ("type", fun f -> s (D.type_ f));
    ("collation", fun f -> s (D.collation f));
    ("engine", fun f -> s (D.engine f));
    ("mongodbObjectId", fun f -> s (D.mongodb_object_id f));
    ( "fake",
      fun f -> s (Faker.Helpers.fake "{{database.column}} {{database.type}} {{database.mongodbObjectId}}" f) );
  ]
