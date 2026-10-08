open T
module W = Faker.Word

type m = ?length:Faker.range -> ?strategy:W.strategy -> Faker.t -> string

let methods : (string * m) list =
  [
    ("adjective", W.adjective);
    ("adverb", W.adverb);
    ("conjunction", W.conjunction);
    ("interjection", W.interjection);
    ("noun", W.noun);
    ("preposition", W.preposition);
    ("verb", W.verb);
    ("sample", W.sample);
  ]

let variants : (string * Faker.range option * W.strategy option) list =
  [
    ("", None, None);
    ("/5", Some (`N 5), None);
    ("/range", Some (`Range (4, 6)), None);
    ("/len3", Some (`N 3), None);
    ("/fail", Some (`N 50), None);
    ("/fail-range", Some (`Range (30, 40)), None);
    ("/closest", Some (`N 30), Some `Closest);
    ("/closest-low", Some (`N 1), Some `Closest);
    ("/closest-range", Some (`Range (25, 28)), Some `Closest);
    ("/shortest", None, Some `Shortest);
    ("/longest", None, Some `Longest);
    ("/shortest-len", Some (`N 50), Some `Shortest);
    ("/longest-len", Some (`Range (40, 50)), Some `Longest);
    ("/any-length", Some (`N 50), Some `Any_length);
    ("/any-length-nolen", None, Some `Any_length);
    ("/fail-nolen", None, Some `Fail);
    ("/closest-nolen", None, Some `Closest);
    ("/match-closest", Some (`Range (5, 7)), Some `Closest);
  ]

let fake p f = s (Faker.Helpers.fake p f)

let cases : case list =
  List.concat_map
    (fun (name, (m : m)) ->
      List.map
        (fun (suffix, length, strategy) ->
          (name ^ suffix, fun f -> s (m ?length ?strategy f)))
        variants)
    methods
  @ [
      ("words", fun f -> s (W.words f));
      ("words/5", fun f -> s (W.words ~count:(`N 5) f));
      ("words/count", fun f -> s (W.words ~count:(`N 4) f));
      ("words/range", fun f -> s (W.words ~count:(`Range (2, 7)) f));
      ("words/0", fun f -> s (W.words ~count:(`N 0) f));
      ( "fake/noun",
        fake
          {|{{word.noun}} {{word.verb(5)}} {{word.adjective({"length":{"min":3,"max":4}})}}|}
      );
      ( "fake/sample",
        fake
          {|{{word.sample}} {{word.sample({"length":40,"strategy":"closest"})}}|}
      );
      ( "fake/misc",
        fake
          {|{{word.adverb}}|{{word.conjunction}}|{{word.interjection}}|{{word.preposition}}|{{word.words(4)}}|{{word.words({"count":{"min":1,"max":2}})}}|}
      );
      ( "fake/strategy",
        fake
          {|{{word.noun({"length":50,"strategy":"longest"})}}|{{word.verb({"strategy":"shortest"})}}|{{word.adjective({"length":50,"strategy":"any-length"})}}|}
      );
    ]
