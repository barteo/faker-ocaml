(* Port of src/modules/word/module.ts and
   src/modules/word/filter-word-list-by-length.ts. *)

type strategy = [ `Fail | `Closest | `Shortest | `Longest | `Any_length ]
(** [LengthStrategyType]: 'fail' | 'closest' | 'shortest' | 'longest' |
    'any-length'. *)

let strategy_of_string = function
  | "fail" -> Some `Fail
  | "closest" -> Some `Closest
  | "shortest" -> Some `Shortest
  | "longest" -> Some `Longest
  | "any-length" -> Some `Any_length
  | _ -> None

(* word.length (UTF-16 code units) *)
let len w = Unicode.js_length w

let filter p (l : string array) =
  Array.of_list (List.filter p (Array.to_list l))

let apply_strategy (strategy : strategy) (word_list : string array) (min, max) :
    string array =
  match strategy with
  | `Fail -> Core.error "No words found that match the given length."
  | `Closest ->
      (* groupBy + Object.keys: the distinct lengths. *)
      let lengths = Array.map (fun w -> float_of_int (len w)) word_list in
      let min_f = float_of_int min and max_f = float_of_int max in
      let closest_below =
        Array.fold_left
          (fun acc l -> if l < min_f then Float.max acc l else acc)
          Float.neg_infinity lengths
      in
      let closest_above =
        Array.fold_left
          (fun acc l -> if l > max_f then Float.min acc l else acc)
          Float.infinity lengths
      in
      let closest_offset =
        Float.min (min_f -. closest_below) (closest_above -. max_f)
      in
      filter
        (fun w ->
          let l = float_of_int (len w) in
          l = min_f -. closest_offset || l = max_f +. closest_offset)
        word_list
  | `Shortest ->
      let m =
        Array.fold_left
          (fun acc w -> Float.min acc (float_of_int (len w)))
          Float.infinity word_list
      in
      filter (fun w -> float_of_int (len w) = m) word_list
  | `Longest ->
      let m =
        Array.fold_left
          (fun acc w -> Float.max acc (float_of_int (len w)))
          Float.neg_infinity word_list
      in
      filter (fun w -> float_of_int (len w) = m) word_list
  | `Any_length -> Array.copy word_list

let filter_word_list_by_length ?length ?(strategy : strategy = `Fail)
    (word_list : string array) : string array =
  match length with
  | Some length -> (
      let p =
        match length with
        | `N n -> fun w -> len w = n
        | `Range (min, max) -> fun w -> len w >= min && len w <= max
      in
      let filtered = filter p word_list in
      if Array.length filtered > 0 then filtered
      else
        match length with
        | `N n -> apply_strategy strategy word_list (n, n)
        | `Range (min, max) -> apply_strategy strategy word_list (min, max))
  | None -> (
      match strategy with
      | (`Shortest | `Longest) as s -> apply_strategy s word_list (0, 0)
      | _ -> Array.copy word_list)

let pick entry ?length ?strategy f =
  let word_list = Locale.strings f "word" entry in
  Fk_helpers.array_element
    (filter_word_list_by_length ?length ?strategy word_list)
    f

let adjective ?length ?strategy f = pick "adjective" ?length ?strategy f
let adverb ?length ?strategy f = pick "adverb" ?length ?strategy f
let conjunction ?length ?strategy f = pick "conjunction" ?length ?strategy f
let interjection ?length ?strategy f = pick "interjection" ?length ?strategy f
let noun ?length ?strategy f = pick "noun" ?length ?strategy f
let preposition ?length ?strategy f = pick "preposition" ?length ?strategy f
let verb ?length ?strategy f = pick "verb" ?length ?strategy f

let sample ?length ?strategy f =
  let methods =
    Fk_helpers.shuffle
      [|
        adjective; adverb; conjunction; interjection; noun; preposition; verb;
      |]
      f
  in
  let rec go i =
    if i >= Array.length methods then
      Core.error "No matching word data available for the current locale"
    else
      match methods.(i) ?length ?strategy f with
      | w -> w
      | exception Core.Faker_error _ -> go (i + 1)
  in
  go 0

let words ?(count = `Range (1, 3)) f =
  String.concat " "
    (Array.to_list (Fk_helpers.multiple ~count (fun _ -> sample f) f))

let registry : (string * Registry.fn) list =
  let open Args in
  let word_fn g f a =
    let o = opts ~shorthand:"length" a in
    str
      (g ?length:(range o "length")
         ?strategy:(Option.bind (string o "strategy") strategy_of_string)
         f)
  in
  [
    ("adjective", word_fn adjective);
    ("adverb", word_fn adverb);
    ("conjunction", word_fn conjunction);
    ("interjection", word_fn interjection);
    ("noun", word_fn noun);
    ("preposition", word_fn preposition);
    ("verb", word_fn verb);
    ("sample", word_fn sample);
    ( "words",
      fun f a ->
        let o = opts ~shorthand:"count" a in
        str (words ?count:(range o "count") f) );
  ]
