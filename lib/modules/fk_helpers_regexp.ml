(* Port of helpers.fromRegExp (src/modules/helpers/module.ts). The regular
   expressions used upstream are re-implemented as small matchers. *)

open Fk_helpers

let boolean f = Fk_datatype.boolean f

let get_repetitions f (symbol : char option) (qmin : string option) (qmax : string option) =
  let doubling_limit () =
    let limit = ref 1 in
    while boolean f do
      limit := !limit * 2
    done;
    !limit
  in
  match symbol with
  | Some '?' -> if boolean f then 0 else 1
  | Some '*' ->
      let limit = doubling_limit () in
      Fk_number.int ~min:0 ~max:limit f
  | Some '+' ->
      let limit = doubling_limit () in
      Fk_number.int ~min:1 ~max:limit f
  | Some _ -> Core.error "Unknown quantifier symbol provided."
  | None -> (
      match (qmin, qmax) with
      | Some a, Some b -> Fk_number.int ~min:(pint a) ~max:(pint b) f
      | Some a, None -> pint a
      | _ -> 1)

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let lower c = String.make 1 (Char.lowercase_ascii c)
let upper c = String.make 1 (Char.uppercase_ascii c)

let replace_unquantified_tokens pattern ci f =
  let b = Buffer.create (String.length pattern) in
  let len = String.length pattern in
  let in_class = ref false in
  let i = ref 0 in
  while !i < len do
    let c = pattern.[!i] in
    (if c = '\\' then begin
       Buffer.add_char b c;
       if !i + 1 < len then begin
         incr i;
         Buffer.add_char b pattern.[!i]
       end
     end
     else if c = '[' then (in_class := true; Buffer.add_char b c)
     else if c = ']' then (in_class := false; Buffer.add_char b c)
     else
       let has_quantifier =
         !i + 1 < len && match pattern.[!i + 1] with '?' | '*' | '+' | '{' -> true | _ -> false
       in
       if (not !in_class) && (not has_quantifier) && c = '.' then
         Buffer.add_string b (Fk_string.alphanumeric f)
       else if (not !in_class) && (not has_quantifier) && ci && is_alpha c then
         Buffer.add_string b (Fk_string.from_character_array [| lower c; upper c |] f)
       else Buffer.add_char b c);
    incr i
  done;
  Buffer.contents b

(* (?:\{(\d+)(?:,(\d+)|)\}|(\?|\*|\+)) at [i]:
   returns (end index, min, max, symbol) *)
let match_quantifier s i =
  let len = String.length s in
  let braces () =
    if i < len && s.[i] = '{' then
      match read_digits s (i + 1) with
      | Some (a, j) when j < len && s.[j] = ',' -> (
          match read_digits s (j + 1) with
          | Some (b, k) when k < len && s.[k] = '}' -> Some (k + 1, Some a, Some b, None)
          | _ -> None)
      | Some (a, j) when j < len && s.[j] = '}' -> Some (j + 1, Some a, None, None)
      | _ -> None
    else None
  in
  match braces () with
  | Some r -> Some r
  | None ->
      if i < len && (s.[i] = '?' || s.[i] = '*' || s.[i] = '+') then
        Some (i + 1, None, None, Some s.[i])
      else None

(* (?![^[]*]|[^{]*}) at [p] *)
let negative_lookahead_ok s p =
  let closes_before ~close ~open_ =
    let rec go i =
      if i >= String.length s then false
      else if s.[i] = close then true
      else if s.[i] = open_ then false
      else go (i + 1)
    in
    go p
  in
  not (closes_before ~close:']' ~open_:'[' || closes_before ~close:'}' ~open_:'{')

(* /([.A-Za-z0-9])(?:\{(\d+)(?:,(\d+)|)\}|(\?|\*|\+))(?![^[]*]|[^{]*})/ *)
let find_single_char s =
  let len = String.length s in
  let rec at i =
    if i >= len then None
    else
      let c = s.[i] in
      if c = '.' || is_alpha c || is_digit c then
        match match_quantifier s (i + 1) with
        | Some (stop, qmin, qmax, sym) when negative_lookahead_ok s stop ->
            Some (i, stop, c, qmin, qmax, sym)
        | _ -> at (i + 1)
      else at (i + 1)
  in
  at 0

(* /\[(\^|)(-|)(.+?)\](?:\{(\d+)(?:,(\d+)|)\}|(\?|\*|\+)|)/ *)
let find_range_alphanumeric s =
  let len = String.length s in
  let lazy_body q =
    (* .+? followed by ]: first ] at index >= q+1, no newline in between *)
    let rec go k =
      if k >= len then None
      else if s.[k - 1] = '\n' then None
      else if s.[k] = ']' then Some k
      else go (k + 1)
    in
    if q >= len then None else go (q + 1)
  in
  let try_at i =
    let options =
      List.concat_map
        (fun neg -> List.map (fun dash -> (neg, dash)) [ true; false ])
        [ true; false ]
    in
    List.find_map
      (fun (neg, dash) ->
        let q = i + 1 in
        if neg && not (q < len && s.[q] = '^') then None
        else
          let q = if neg then q + 1 else q in
          if dash && not (q < len && s.[q] = '-') then None
          else
            let q = if dash then q + 1 else q in
            match lazy_body q with
            | None -> None
            | Some close ->
                let body = String.sub s q (close - q) in
                let stop, qmin, qmax, sym =
                  match match_quantifier s (close + 1) with
                  | Some r -> r
                  | None -> (close + 1, None, None, None)
                in
                Some (i, stop, neg, dash, body, qmin, qmax, sym))
      options
  in
  let rec at i =
    if i >= len then None
    else if s.[i] = '[' then match try_at i with Some r -> Some r | None -> at (i + 1)
    else at (i + 1)
  in
  at 0

(* SINGLE_RANGE_REG: digit-digit, word-word, digit, word or one of the specials; returns the matched text *)
let find_single_range s =
  let len = String.length s in
  let special c = String.contains "-!@#$&()`.+,/\"" c in
  let rec at i =
    if i >= len then None
    else
      let c = s.[i] in
      if i + 2 < len && is_digit c && s.[i + 1] = '-' && is_digit s.[i + 2] then Some (String.sub s i 3)
      else if i + 2 < len && is_word c && s.[i + 1] = '-' && is_word s.[i + 2] then Some (String.sub s i 3)
      else if is_word c || special c then Some (String.make 1 c)
      else at (i + 1)
  in
  at 0

let utf8_of_code cp =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int cp);
  Buffer.contents b

(** [from_reg_exp ?flags pattern]. When [flags] is given the pattern is treated
    like a JavaScript [RegExp] source: leading [^] and trailing [$] are removed and
    the [i] flag enables case-insensitive generation. *)
let from_reg_exp ?flags pattern f =
  let ci, pattern =
    match flags with
    | None -> (false, pattern)
    | Some fl ->
        let p = ref pattern in
        while String.length !p > 0 && !p.[0] = '^' do
          p := String.sub !p 1 (String.length !p - 1)
        done;
        while String.length !p > 0 && !p.[String.length !p - 1] = '$' do
          p := String.sub !p 0 (String.length !p - 1)
        done;
        (String.contains fl 'i', !p)
  in
  if pattern = "." then Fk_string.alphanumeric f
  else if ci && String.length pattern = 1 && is_alpha pattern.[0] then
    Fk_string.from_character_array [| lower pattern.[0]; upper pattern.[0] |] f
  else begin
    let pattern = ref (replace_unquantified_tokens pattern ci f) in
    let rec single_loop () =
      match find_single_char !pattern with
      | None -> ()
      | Some (i, stop, c, qmin, qmax, sym) ->
          let reps = get_repetitions f sym qmin qmax in
          let replacement =
            if c = '.' then Fk_string.alphanumeric ~length:(`N reps) f
            else if ci then Fk_string.from_character_array ~length:(`N reps) [| lower c; upper c |] f
            else Js.repeat (String.make 1 c) reps
          in
          pattern := splice !pattern i stop replacement;
          single_loop ()
    in
    single_loop ();
    let rec range_loop () =
      match find_range_alphanumeric !pattern with
      | None -> ()
      | Some (i, stop, neg, dash, body, qmin, qmax, sym) ->
          let codes = ref [] in
          (* built in reverse *)
          let push x = codes := x :: !codes in
          if dash then push 45;
          let ranges = ref body in
          let rec ranges_loop () =
            match find_single_range !ranges with
            | None -> ()
            | Some m ->
                (if String.contains m '-' then begin
                   if String.length m = 3 then begin
                     let min = Char.code m.[0] and max = Char.code m.[2] in
                     if min > max then Core.error "Character range provided is out of order.";
                     for i = min to max do
                       let ch = Char.chr i in
                       if ci && not (is_digit ch) then begin
                         push (Char.code (Char.uppercase_ascii ch));
                         push (Char.code (Char.lowercase_ascii ch))
                       end
                       else push i
                     done
                   end
                   (* a lone '-' yields NaN bounds upstream and adds nothing *)
                 end
                 else
                   let ch = m.[0] in
                   if ci && not (is_digit ch) then begin
                     push (Char.code (Char.uppercase_ascii ch));
                     push (Char.code (Char.lowercase_ascii ch))
                   end
                   else push (Char.code ch));
                ranges := Js.substring !ranges (String.length m) (String.length !ranges);
                ranges_loop ()
          in
          ranges_loop ();
          let reps = get_repetitions f sym qmin qmax in
          let codes = ref (List.rev !codes) in
          if neg then begin
            let toggle i =
              if List.mem i !codes then begin
                (* remove first occurrence *)
                let rec rm = function [] -> [] | x :: xs -> if x = i then xs else x :: rm xs in
                codes := rm !codes
              end
              else codes := !codes @ [ i ]
            in
            for i = 48 to 57 do toggle i done;
            for i = 65 to 90 do toggle i done;
            for i = 97 to 122 do toggle i done
          end;
          let arr = Array.of_list !codes in
          let generated =
            String.concat ""
              (Array.to_list
                 (multiple ~count:(`N reps) (fun _ -> utf8_of_code (array_element arr f)) f))
          in
          pattern := splice !pattern i stop generated;
          range_loop ()
    in
    range_loop ();
    let rec rep_range_loop () =
      match find_range_rep !pattern with
      | None -> ()
      | Some (i, j, c, a, b) ->
          let min = pint a and max = pint b in
          if min > max then Core.error "Numbers out of order in {} quantifier.";
          let reps = Fk_number.int ~min ~max f in
          pattern := splice !pattern i j (Js.repeat (String.make 1 c) reps);
          rep_range_loop ()
    in
    rep_range_loop ();
    let rec rep_loop () =
      match find_rep !pattern with
      | None -> ()
      | Some (i, j, c, a) ->
          pattern := splice !pattern i j (Js.repeat (String.make 1 c) (pint a));
          rep_loop ()
    in
    rep_loop ();
    !pattern
  end
