(* Unicode helpers emulating String.prototype.normalize('NFKD') for the
   blocks covered by Unicode_table. *)

let table =
  lazy
    (let h = Hashtbl.create 2048 in
     Array.iter (fun (cp, d) -> Hashtbl.replace h cp d) Unicode_table.nfkd;
     h)

let fold_uchars s fn =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      fn b (Uchar.to_int (Uchar.utf_decode_uchar d)) (String.sub s i n);
      go (i + n)
    end
  in
  go 0;
  Buffer.contents b

let nfkd s =
  let t = Lazy.force table in
  fold_uchars s (fun b cp raw ->
      match Hashtbl.find_opt t cp with
      | Some d -> Buffer.add_string b d
      | None -> Buffer.add_string b raw)

(* Removes U+0300..U+036F. *)
let strip_combining_marks s =
  fold_uchars s (fun b cp raw ->
      if cp < 0x300 || cp > 0x36F then Buffer.add_string b raw)

let ccc_table =
  lazy
    (let h = Hashtbl.create 1024 in
     Array.iter (fun (cp, r) -> Hashtbl.replace h cp r) Unicode_ccc.rank;
     h)

(** Canonical ordering (the last step of NFD/NFKD): each run of combining marks
    is stably sorted by canonical combining class. *)
let canonical_order s =
  let t = Lazy.force ccc_table in
  let ccc cp = Option.value (Hashtbl.find_opt t cp) ~default:0 in
  let out = Buffer.create (String.length s) in
  let run = ref [] in
  let flush () =
    List.iter
      (fun (_, raw) -> Buffer.add_string out raw)
      (List.stable_sort (fun (a, _) (b, _) -> compare a b) (List.rev !run));
    run := []
  in
  ignore
    (fold_uchars s (fun _ cp raw ->
         let c = ccc cp in
         if c = 0 then (
           flush ();
           Buffer.add_string out raw)
         else run := (c, raw) :: !run));
  flush ();
  Buffer.contents out

(* ---------- case mapping (String.prototype.toUpperCase / toLowerCase) ---------- *)

let case_table rows =
  lazy
    (let h = Hashtbl.create 2048 in
     Array.iter (fun (cp, m) -> Hashtbl.replace h cp m) rows;
     h)

let upper_table = case_table Unicode_case.upper
let lower_table = case_table Unicode_case.lower

let in_ranges ranges cp =
  let rec go lo hi =
    if lo > hi then false
    else
      let mid = (lo + hi) / 2 in
      let a, b = ranges.(mid) in
      if cp < a then go lo (mid - 1)
      else if cp > b then go (mid + 1) hi
      else true
  in
  go 0 (Array.length ranges - 1)

let is_cased = in_ranges Unicode_case.cased
let is_case_ignorable = in_ranges Unicode_case.case_ignorable

(** [js_upper s] is [s.toUpperCase()]. *)
let js_upper s =
  let t = Lazy.force upper_table in
  fold_uchars s (fun b cp raw ->
      Buffer.add_string b (Option.value (Hashtbl.find_opt t cp) ~default:raw))

let code_points s =
  let l = ref [] in
  ignore (fold_uchars s (fun _ cp _ -> l := cp :: !l));
  Array.of_list (List.rev !l)

(** [js_lower s] is [s.toLowerCase()], including the final-sigma rule. *)
let js_lower s =
  let t = Lazy.force lower_table in
  let cps = code_points s in
  let n = Array.length cps in
  (* A cased letter before [i] (skipping case-ignorables), and none after it. *)
  let final_sigma i =
    let rec before j =
      j >= 0
      && if is_case_ignorable cps.(j) then before (j - 1) else is_cased cps.(j)
    in
    let rec after j =
      j < n
      && if is_case_ignorable cps.(j) then after (j + 1) else is_cased cps.(j)
    in
    before (i - 1) && not (after (i + 1))
  in
  let b = Buffer.create (String.length s) in
  Array.iteri
    (fun i cp ->
      if cp = 0x3A3 && final_sigma i then Buffer.add_string b "\u{3C2}"
      else
        match Hashtbl.find_opt t cp with
        | Some m -> Buffer.add_string b m
        | None -> Buffer.add_utf_8_uchar b (Uchar.of_int cp))
    cps;
  Buffer.contents b

(** [js_upper_first s] is [s.charAt(0).toUpperCase() + s.slice(1)]. [charAt(0)]
    of an astral character is a lone surrogate, which [toUpperCase] leaves
    alone. *)
let js_upper_first s =
  if s = "" then s
  else
    let d = String.get_utf_8_uchar s 0 in
    let n = Uchar.utf_decode_length d in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    if cp >= 0x10000 then s
    else js_upper (String.sub s 0 n) ^ String.sub s n (String.length s - n)

(* Number of UTF-16 code units (JavaScript string length). *)
let js_length s =
  let n = ref 0 in
  ignore
    (fold_uchars s (fun _ cp _ -> n := !n + if cp >= 0x10000 then 2 else 1));
  !n
