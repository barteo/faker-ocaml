(* Port of src/modules/music/module.ts. *)

let pick entry f = Fk_helpers.array_element (Locale.strings f "music" entry) f

let album f = pick "album" f
let artist f = pick "artist" f
let genre f = pick "genre" f
let song_name f = pick "song_name" f

let registry : (string * Registry.fn) list =
  [
    ("album", fun f _ -> Json.Str (album f));
    ("artist", fun f _ -> Json.Str (artist f));
    ("genre", fun f _ -> Json.Str (genre f));
    ("songName", fun f _ -> Json.Str (song_name f));
  ]
