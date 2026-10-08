(* Port of src/modules/lorem/module.ts. *)

let word ?length ?strategy f =
  let word_list = Locale.strings f "lorem" "word" in
  Fk_helpers.array_element (Fk_word.filter_word_list_by_length ?length ?strategy word_list) f

let join sep a = String.concat sep (Array.to_list a)

(** [words ?word_count f]: [word_count] defaults to 3. *)
let words ?(word_count = `N 3) f = join " " (Fk_helpers.multiple ~count:word_count (fun _ -> word f) f)

(* sentence.charAt(0).toUpperCase() + sentence.substring(1) *)
let upper_first = Unicode.js_upper_first

(** [sentence ?word_count f]: [word_count] defaults to 3..10. *)
let sentence ?(word_count = `Range (3, 10)) f = upper_first (words ~word_count f) ^ "."

let slug ?(word_count = `N 3) f = Fk_helpers.slugify (words ~word_count f)

(** [sentences ?sentence_count ?separator f]: 2..6 sentences joined by [" "]. *)
let sentences ?(sentence_count = `Range (2, 6)) ?(separator = " ") f =
  join separator (Fk_helpers.multiple ~count:sentence_count (fun _ -> sentence f) f)

let paragraph ?(sentence_count = `N 3) f = sentences ~sentence_count f

(** [paragraphs ?paragraph_count ?separator f]: 3 paragraphs joined by ["\n"]. *)
let paragraphs ?(paragraph_count = `N 3) ?(separator = "\n") f =
  join separator (Fk_helpers.multiple ~count:paragraph_count (fun _ -> paragraph f) f)

let lines ?(line_count = `Range (1, 5)) f = sentences ~sentence_count:line_count ~separator:"\n" f

let text f =
  let methods =
    [|
      (fun f -> sentence f);
      (fun f -> sentences f);
      (fun f -> paragraph f);
      (fun f -> paragraphs f);
      (fun f -> lines f);
    |]
  in
  (Fk_helpers.array_element methods f) f

let registry : (string * Registry.fn) list =
  let open Args in
  (* positional NumberOrRange argument *)
  let count a i =
    match nth a i with
    | Some v -> range [ ("c", v) ] "c"
    | None -> None
  in
  let sep a i = match nth a i with Some (Json.Str s) -> Some s | _ -> None in
  [
    ( "word",
      fun f a ->
        let o = opts ~shorthand:"length" a in
        str
          (word ?length:(range o "length")
             ?strategy:(Option.bind (string o "strategy") Fk_word.strategy_of_string)
             f) );
    ("words", fun f a -> str (words ?word_count:(count a 0) f));
    ("sentence", fun f a -> str (sentence ?word_count:(count a 0) f));
    ("slug", fun f a -> str (slug ?word_count:(count a 0) f));
    ("sentences", fun f a -> str (sentences ?sentence_count:(count a 0) ?separator:(sep a 1) f));
    ("paragraph", fun f a -> str (paragraph ?sentence_count:(count a 0) f));
    ( "paragraphs",
      fun f a -> str (paragraphs ?paragraph_count:(count a 0) ?separator:(sep a 1) f) );
    ("text", fun f _ -> str (text f));
    ("lines", fun f a -> str (lines ?line_count:(count a 0) f));
  ]
