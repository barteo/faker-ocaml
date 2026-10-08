// Supplementary NFKD table for code points not covered by lib/internal/unicode_table.ml
// (Hangul syllables are decomposed algorithmically in OCaml).
const fs = require('fs');
const cov = [[0x80, 0x52f], [0x1e00, 0x1fff], [0x2000, 0x218f], [0x2460, 0x24ff], [0x3000, 0x30ff], [0xfb00, 0xfb4f], [0xff00, 0xffef]];
const esc = (s) => '"' + [...s].map((c) => { const cp = c.codePointAt(0); if (cp < 0x20 || cp >= 0x7f) return `\\u{${cp.toString(16)}}`; if (c === '"' || c === '\\') return '\\' + c; return c; }).join('') + '"';
const rows = [];
for (let cp = 0x80; cp <= 0x10ffff; cp++) {
  if (cp >= 0xd800 && cp <= 0xdfff) continue;
  if (cp >= 0xac00 && cp <= 0xd7a3) continue;
  if (cov.some(([a, b]) => cp >= a && cp <= b)) continue;
  const s = String.fromCodePoint(cp);
  const d = s.normalize('NFKD');
  if (d !== s) rows.push(`    (0x${cp.toString(16)}, ${esc(d)});`);
}
fs.writeFileSync(process.argv[2],
`(* GENERATED (Node ${process.version}): NFKD decompositions of the code points that
   lib/internal/unicode_table.ml does not cover (Arabic, Armenian, CJK
   compatibility, mathematical alphanumerics, presentation forms, ...).
   Hangul syllables are decomposed algorithmically by [nfkd]. Used by
   [Fk_internet.username] to emulate String.prototype.normalize('NFKD') on
   arbitrary user-provided names. *)

let table : (int * string) array =
  [|
${rows.join('\n')}
  |]

let tbl =
  lazy
    (let h = Hashtbl.create 8192 in
     Array.iter (fun (k, v) -> Hashtbl.replace h k v) table;
     h)

(* Hangul syllable decomposition (Unicode 3.12). *)
let hangul b cp =
  let s = cp - 0xAC00 in
  let l = 0x1100 + (s / 588) and v = 0x1161 + (s mod 588 / 28) and t = 0x11A7 + (s mod 28) in
  Buffer.add_utf_8_uchar b (Uchar.of_int l);
  Buffer.add_utf_8_uchar b (Uchar.of_int v);
  if t <> 0x11A7 then Buffer.add_utf_8_uchar b (Uchar.of_int t)

(** String.prototype.normalize('NFKD') (without canonical reordering of
    combining marks). *)
let nfkd s =
  let t = Lazy.force tbl in
  Unicode.nfkd
    (Unicode.fold_uchars s (fun b cp raw ->
         if cp >= 0xAC00 && cp <= 0xD7A3 then hangul b cp
         else match Hashtbl.find_opt t cp with Some d -> Buffer.add_string b d | None -> Buffer.add_string b raw))
`);
console.log(rows.length);
