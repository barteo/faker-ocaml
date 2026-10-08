(* Port of src/modules/git/module.ts. Dates are float epoch milliseconds. *)

type eol = [ `LF | `CRLF ]

let nbsp = "\xc2\xa0"

(** [commit_sha ?length f]: a lowercase hex SHA, [length] defaults to 40. *)
let commit_sha ?(length = 40) f =
  Fk_string.hexadecimal ~length:(`N length) ~casing:`Lower ~prefix:"" f

let branch f =
  let noun = Js.replace_first ~sub:" " ~by:"-" (Fk_hacker.noun f) in
  let verb = Js.replace_first ~sub:" " ~by:"-" (Fk_hacker.verb f) in
  noun ^ "-" ^ verb

let commit_message f =
  let verb = Fk_hacker.verb f in
  let adjective = Fk_hacker.adjective f in
  let noun = Fk_hacker.noun f in
  verb ^ " " ^ adjective ^ " " ^ noun

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let months =
  [|
    "Jan";
    "Feb";
    "Mar";
    "Apr";
    "May";
    "Jun";
    "Jul";
    "Aug";
    "Sep";
    "Oct";
    "Nov";
    "Dec";
  |]

let commit_date_input ?ref_date f =
  let date = Fk_date.recent_input ~days:(`N 1) ?ref_date f in
  let p = Date_util.to_parts date in
  let pad2 n = Js.pad_start (string_of_int n) 2 '0' in
  let timezone = Fk_number.int ~min:(-11) ~max:12 f in
  Printf.sprintf "%s %s %d %s:%s:%s %d %s%s00" days.(p.weekday) months.(p.month)
    p.day (pad2 p.hours) (pad2 p.minutes) (pad2 p.seconds) p.year
    (if timezone >= 0 then "+" else "-")
    (pad2 (abs timezone))

(** [commit_date ?ref_date f]: a date within one day before [ref_date] in git's
    default format, e.g. ["Thu Jan 1 00:00:00 1970 +0000"] (random timezone). *)
let commit_date ?ref_date f =
  commit_date_input ?ref_date:(Fk_date.num_opt ref_date) f

(* /^[.,:;"\\']|[<>\n]|[.,:;"\\']$/g -> '' *)
let normalize_user user =
  let n = String.length user in
  let edge c = String.contains ".,:;\"\\'" c in
  let b = Buffer.create n in
  String.iteri
    (fun i c ->
      let drop =
        (i = 0 && edge c)
        || c = '<' || c = '>' || c = '\n'
        || (i = n - 1 && edge c)
      in
      if not drop then Buffer.add_char b c)
    user;
  Buffer.contents b

let commit_entry_input ?merge ?(eol : eol = `CRLF) ?ref_date f =
  let merge =
    match merge with
    | Some m -> m
    | None -> Fk_datatype.boolean ~probability:0.2 f
  in
  let commit = "commit " ^ commit_sha f in
  let merge_line =
    if merge then
      let a = commit_sha ~length:7 f in
      let b = commit_sha ~length:7 f in
      [ "Merge: " ^ a ^ " " ^ b ]
    else []
  in
  let first_name = Fk_person.first_name f in
  let last_name = Fk_person.last_name f in
  let full_name = Fk_person.full_name ~first_name ~last_name f in
  let username = Fk_internet.username ~first_name ~last_name f in
  let user = Fk_helpers.array_element [| full_name; username |] f in
  let email = Fk_internet.email ~first_name ~last_name f in
  let user = normalize_user user in
  let author = "Author: " ^ user ^ " <" ^ email ^ ">" in
  let date = "Date: " ^ commit_date_input ?ref_date f in
  let message = Js.repeat nbsp 4 ^ commit_message f in
  let lines = (commit :: merge_line) @ [ author; date; ""; message; "" ] in
  String.concat (match eol with `CRLF -> "\r\n" | `LF -> "\n") lines

(** [commit_entry ?merge ?eol ?ref_date f]: a [git log] entry. [merge] defaults
    to a 20% chance, [eol] to [`CRLF]. *)
let commit_entry ?merge ?eol ?ref_date f =
  commit_entry_input ?merge ?eol ?ref_date:(Fk_date.num_opt ref_date) f

let registry : (string * Registry.fn) list =
  let open Args in
  [
    ("branch", fun f _ -> str (branch f));
    ( "commitEntry",
      fun f a ->
        let o = opts a in
        let eol =
          match string o "eol" with Some "LF" -> Some `LF | _ -> None
        in
        str
          (commit_entry_input ?merge:(bool o "merge") ?eol
             ?ref_date:(Fk_date.ref_arg o) f) );
    ("commitMessage", fun f _ -> str (commit_message f));
    ( "commitDate",
      fun f a ->
        let o = opts a in
        str (commit_date_input ?ref_date:(Fk_date.ref_arg o) f) );
    ( "commitSha",
      fun f a ->
        let o = opts a in
        str (commit_sha ?length:(int o "length") f) );
  ]
