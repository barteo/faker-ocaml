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
      match Hashtbl.find_opt t cp with Some d -> Buffer.add_string b d | None -> Buffer.add_string b raw)

(* Removes U+0300..U+036F. *)
let strip_combining_marks s =
  fold_uchars s (fun b cp raw -> if cp < 0x300 || cp > 0x36F then Buffer.add_string b raw)

(* Number of UTF-16 code units (JavaScript string length). *)
let js_length s =
  let n = ref 0 in
  ignore (fold_uchars s (fun _ cp _ -> n := !n + if cp >= 0x10000 then 2 else 1));
  !n
