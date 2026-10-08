open T
module L = Faker.Lorem

let fake p f = s (Faker.Helpers.fake p f)

let cases : case list =
  [
    ("word", fun f -> s (L.word f));
    ("word/5", fun f -> s (L.word ~length:(`N 5) f));
    ("word/range", fun f -> s (L.word ~length:(`Range (2, 4)) f));
    ("word/fail", fun f -> s (L.word ~length:(`N 30) f));
    ("word/closest", fun f -> s (L.word ~length:(`N 30) ~strategy:`Closest f));
    ("word/shortest", fun f -> s (L.word ~strategy:`Shortest f));
    ("word/longest", fun f -> s (L.word ~strategy:`Longest f));
    ("word/any-length", fun f -> s (L.word ~length:(`N 30) ~strategy:`Any_length f));
    ("word/longest-len", fun f -> s (L.word ~length:(`N 30) ~strategy:`Longest f));
    ("words", fun f -> s (L.words f));
    ("words/7", fun f -> s (L.words ~word_count:(`N 7) f));
    ("words/range", fun f -> s (L.words ~word_count:(`Range (1, 10)) f));
    ("words/0", fun f -> s (L.words ~word_count:(`N 0) f));
    ("sentence", fun f -> s (L.sentence f));
    ("sentence/5", fun f -> s (L.sentence ~word_count:(`N 5) f));
    ("sentence/range", fun f -> s (L.sentence ~word_count:(`Range (1, 3)) f));
    ("sentence/0", fun f -> s (L.sentence ~word_count:(`N 0) f));
    ("slug", fun f -> s (L.slug f));
    ("slug/5", fun f -> s (L.slug ~word_count:(`N 5) f));
    ("slug/range", fun f -> s (L.slug ~word_count:(`Range (1, 4)) f));
    ("sentences", fun f -> s (L.sentences f));
    ("sentences/2", fun f -> s (L.sentences ~sentence_count:(`N 2) f));
    ("sentences/range", fun f -> s (L.sentences ~sentence_count:(`Range (1, 3)) f));
    ("sentences/sep", fun f -> s (L.sentences ~sentence_count:(`N 3) ~separator:"\n" f));
    ("sentences/sep-range", fun f -> s (L.sentences ~sentence_count:(`Range (2, 4)) ~separator:" | " f));
    ("paragraph", fun f -> s (L.paragraph f));
    ("paragraph/5", fun f -> s (L.paragraph ~sentence_count:(`N 5) f));
    ("paragraph/range", fun f -> s (L.paragraph ~sentence_count:(`Range (1, 2)) f));
    ("paragraphs", fun f -> s (L.paragraphs f));
    ("paragraphs/2", fun f -> s (L.paragraphs ~paragraph_count:(`N 2) f));
    ("paragraphs/range", fun f -> s (L.paragraphs ~paragraph_count:(`Range (1, 3)) f));
    ("paragraphs/sep", fun f -> s (L.paragraphs ~paragraph_count:(`N 2) ~separator:"<br/>\n" f));
    ("text", fun f -> s (L.text f));
    ("lines", fun f -> s (L.lines f));
    ("lines/3", fun f -> s (L.lines ~line_count:(`N 3) f));
    ("lines/range", fun f -> s (L.lines ~line_count:(`Range (2, 4)) f));
    ("fake/basic", fake "{{lorem.word}}|{{lorem.words}}|{{lorem.sentence}}|{{lorem.slug}}|{{lorem.text}}");
    ( "fake/args",
      fake
        {|{{lorem.word(5)}}|{{lorem.word({"length":30,"strategy":"closest"})}}|{{lorem.words(2)}}|{{lorem.sentence({"min":1,"max":2})}}|{{lorem.slug(2)}}|}
    );
    ( "fake/multi",
      fake
        {|{{lorem.sentences(2, "--")}}|{{lorem.paragraph(1)}}|{{lorem.paragraphs(2, "##")}}|{{lorem.lines(2)}}|{{lorem.sentences}}|{{lorem.paragraphs}}|{{lorem.lines}}|{{lorem.paragraph}}|}
    );
  ]
