open T
module M = Faker.Music

let cases : case list =
  [
    ("album", fun f -> s (M.album f));
    ("artist", fun f -> s (M.artist f));
    ("genre", fun f -> s (M.genre f));
    ("songName", fun f -> s (M.song_name f));
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{music.album}}|{{music.artist}}|{{music.genre}}|{{music.songName}}|"
             f) );
  ]
