const fs = require('fs');
let src = fs.readFileSync(process.argv[2], 'utf8');
src = src.replace(/: \{ \[key: string\]: string \}/g, '').replace(/export const/g, 'const');
const charMapping = new Function(src + '; return charMapping;')();
const entries = Object.entries(charMapping);
let single = 0, multi = 0, empty = 0, nonascii = 0;
const out = [];
for (const [k, v] of entries) {
  if ([...k].length !== 1) { multi++; continue; }
  if (!v) { empty++; continue; }
  if (/[^\x00-\x7f]/.test(v)) nonascii++;
  single++;
  out.push(`    (0x${k.codePointAt(0).toString(16).toUpperCase()}, ${JSON.stringify(v)});`);
}
console.error({ single, multi, empty, nonascii, total: entries.length });
fs.writeFileSync(process.argv[3],
`(* GENERATED from upstream src/modules/internet/char-mappings.ts: the merged
   [charMapping] table restricted to single-code-point keys with a non-empty
   value (the only entries [internet.username] can ever use, since it looks up
   one code point at a time and treats '' as missing). *)

let table : (int * string) array =
  [|
${out.join('\n')}
  |]

let lookup =
  let h = lazy (let h = Hashtbl.create 512 in Array.iter (fun (k, v) -> Hashtbl.replace h k v) table; h) in
  fun cp -> Hashtbl.find_opt (Lazy.force h) cp
`);
