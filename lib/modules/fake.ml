(* Port of helpers.fake (src/modules/helpers/module.ts) and fakeEval
   (src/modules/helpers/eval.ts), resolving names through Registry. *)

type value =
  | Root  (** the faker instance *)
  | Module of string  (** faker.<module> *)
  | Fn of (Json.t list -> Json.t)
  | Data of Json.t
  | Undefined

let rec resolve_property (f : Core.t) (entry : value) key : value =
  match entry with
  | Fn g -> ( match g [] with v -> resolve_property f (Data v) key | exception _ -> Undefined)
  | Root -> if Registry.find_module key <> None then Module key else Undefined
  | Module m -> (
      match Registry.find_method m key with Some fn -> Fn (fn f) | None -> Undefined)
  | Data (Json.Obj _ as o) -> (
      match Json.member key o with Some v -> Data v | None -> Undefined)
  | Data (Json.Arr a) -> (
      match int_of_string_opt key with
      | Some i when i >= 0 && i < Array.length a -> Data a.(i)
      | _ -> if key = "length" then Data (Json.int (Array.length a)) else Undefined)
  | Data _ | Undefined -> Undefined

let find_params input : int * Json.t list =
  let index = Js.index_of ~from:1 ~sub:")" input in
  if index = -1 then Core.error "Missing closing parenthesis in '%s'" input;
  let has_quote s = String.contains s '\'' || String.contains s '"' in
  let rec go index =
    if index = -1 then
      let index = String.rindex input ')' in
      (index, [ Json.Str (String.sub input 1 (index - 1)) ])
    else
      let params = String.sub input 1 (index - 1) in
      match Json.parse ("[" ^ params ^ "]") with
      | Json.Arr a -> (index, Array.to_list a)
      | _ -> go (Js.index_of ~from:(index + 1) ~sub:")" input)
      | exception Json.Parse_error _ -> (
          let retry =
            if has_quote params then None
            else
              match Json.parse ("[\"" ^ params ^ "\"]") with
              | Json.Arr a -> Some (index, Array.to_list a)
              | _ -> None
              | exception Json.Parse_error _ -> None
          in
          match retry with Some r -> r | None -> go (Js.index_of ~from:(index + 1) ~sub:")" input))
  in
  go index

let fake_eval (expression : string) (f : Core.t) : Json.t =
  if expression = "" then Core.error "Eval expression cannot be empty.";
  let rec loop (current : value list) remaining =
    let index, current =
      if Js.starts_with ~prefix:"(" remaining then begin
        let index, params = find_params remaining in
        let next = if index + 1 < String.length remaining then Some remaining.[index + 1] else None in
        (match next with
        | Some '.' | Some '(' | None -> ()
        | Some c ->
            Core.error
              "Expected dot ('.'), open parenthesis ('('), or nothing after function call but got \
               '%c'"
              c);
        ( (index + if next = Some '.' then 2 else 1),
          List.map (function Fn g -> Data (g params) | _ -> Undefined) current )
      end
      else begin
        let len = String.length remaining in
        let rec find i = if i >= len || remaining.[i] = '.' || remaining.[i] = '(' then i else find (i + 1) in
        let index = find 0 in
        let dot_match = index < len && remaining.[index] = '.' in
        let key = String.sub remaining 0 index in
        if key = "" then Core.error "Expression parts cannot be empty in '%s'" remaining;
        let next = if index + 1 < len then Some remaining.[index + 1] else None in
        if dot_match && (next = None || next = Some '.' || next = Some '(') then
          Core.error "Found dot without property name in '%s'" remaining;
        ((index + if dot_match then 1 else 0), List.map (fun e -> resolve_property f e key) current)
      end
    in
    let remaining = Js.substring remaining index (String.length remaining) in
    let current =
      List.filter_map
        (function
          | Undefined | Data Json.Null -> None
          | Data (Json.Arr a) -> Some (Data (Fk_helpers.array_element a f))
          | v -> Some v)
        current
    in
    if remaining <> "" && current <> [] then loop current remaining else current
  in
  match loop [ Root; Data f.locale ] expression with
  | [] -> Core.error "Cannot resolve expression '%s'" expression
  | v :: _ -> (
      match v with
      | Fn g -> g []
      | Data d -> d
      | Root | Module _ -> Json.Str "[object Object]"
      | Undefined -> Json.Null)

(* pattern.search(/{{[a-z]/) *)
let search_start pattern =
  let len = String.length pattern in
  let rec go i =
    if i + 2 >= len then -1
    else if pattern.[i] = '{' && pattern.[i + 1] = '{' && pattern.[i + 2] >= 'a' && pattern.[i + 2] <= 'z'
    then i
    else go (i + 1)
  in
  go 0

let rec fake (pattern : string) (f : Core.t) : string =
  let start = search_start pattern in
  let end_ = Js.index_of ~from:start ~sub:"}}" pattern in
  if start = -1 || end_ = -1 then pattern
  else
    let token = Js.substring pattern (start + 2) (end_ + 2) in
    let meth = Js.replace_first ~sub:"{{" ~by:"" (Js.replace_first ~sub:"}}" ~by:"" token) in
    let result = fake_eval meth f in
    let patched =
      String.sub pattern 0 start ^ Json.to_js_string result
      ^ Js.substring pattern (end_ + 2) (String.length pattern)
    in
    fake patched f

let fake_one_of (patterns : string array) f = fake (Fk_helpers.array_element patterns f) f

(** Picks one of the string patterns of a locale entry (string or array) and fakes it. *)
let fake_json (patterns : Json.t) f =
  match patterns with
  | Json.Arr a -> fake (Locale.to_string (Fk_helpers.array_element a f)) f
  | v -> fake (Locale.to_string v) f

type mustache_value = [ `S of string | `F of string -> string ]

let mustache (text : string option) (data : (string * mustache_value) list) =
  match text with
  | None -> ""
  | Some text ->
      List.fold_left
        (fun text (p, v) ->
          let sub = "{{" ^ p ^ "}}" in
          match v with
          | `S s -> Js.replace_all ~sub ~by:s text
          | `F fn ->
              let b = Buffer.create (String.length text) in
              let sl = String.length sub in
              let rec go i =
                if i > String.length text - sl then Buffer.add_string b (Js.slice text i)
                else if String.sub text i sl = sub then (Buffer.add_string b (fn sub); go (i + sl))
                else (Buffer.add_char b text.[i]; go (i + 1))
              in
              go 0;
              Buffer.contents b)
        text data
