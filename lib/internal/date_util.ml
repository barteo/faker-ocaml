(* JavaScript Date emulation in UTC. A date is a float of epoch milliseconds. *)

let ms_per_day = 86400000.0

(* Howard Hinnant's days_from_civil / civil_from_days. *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = (((153 * mp) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((if m <= 2 then y + 1 else y), m, d)

type parts = {
  year : int;
  month : int;  (** 0-11, like getUTCMonth *)
  day : int;  (** 1-31 *)
  hours : int;
  minutes : int;
  seconds : int;
  ms : int;
  weekday : int;  (** 0 = Sunday *)
}

let floor_div a b = int_of_float (Float.floor (a /. b))

let to_parts (t : float) : parts =
  let days = floor_div t ms_per_day in
  let rem = int_of_float (t -. (float_of_int days *. ms_per_day)) in
  let year, m, day = civil_from_days days in
  let weekday = ((days mod 7) + 11) mod 7 in
  {
    year;
    month = m - 1;
    day;
    hours = rem / 3600000;
    minutes = rem / 60000 mod 60;
    seconds = rem / 1000 mod 60;
    ms = rem mod 1000;
    weekday;
  }

(* Date.UTC(year, month0, day, h, mi, s, ms) *)
let utc ?(hours = 0) ?(minutes = 0) ?(seconds = 0) ?(ms = 0) year month0 day =
  let m = ((month0 mod 12) + 12) mod 12 in
  let y = year + ((month0 - m) / 12) in
  let days = days_from_civil y (m + 1) 1 + (day - 1) in
  (float_of_int days *. ms_per_day)
  +. float_of_int ((hours * 3600000) + (minutes * 60000) + (seconds * 1000) + ms)

(** Date.prototype.toISOString *)
let to_iso (t : float) =
  if Float.is_nan t || Float.abs t > 8.64e15 then
    raise (Invalid_argument "Invalid time value");
  let p = to_parts t in
  let year =
    if p.year >= 0 && p.year <= 9999 then Printf.sprintf "%04d" p.year
    else Printf.sprintf "%c%06d" (if p.year < 0 then '-' else '+') (abs p.year)
  in
  Printf.sprintf "%s-%02d-%02dT%02d:%02d:%02d.%03dZ" year (p.month + 1) p.day
    p.hours p.minutes p.seconds p.ms

(** Parses the ISO formats accepted by [new Date(string)] (UTC unless an offset
    is given). Date-only forms are UTC, like in JavaScript. *)
let of_iso (s : string) : float =
  let fail () = invalid_arg ("Invalid date: " ^ s) in
  let len = String.length s in
  let pos = ref 0 in
  let num n =
    if !pos + n > len then fail ();
    let v = String.sub s !pos n in
    String.iter (fun c -> if c < '0' || c > '9' then fail ()) v;
    pos := !pos + n;
    int_of_string v
  in
  let accept c =
    if !pos < len && s.[!pos] = c then (
      incr pos;
      true)
    else false
  in
  let year =
    if accept '+' then num 6 else if accept '-' then -num 6 else num 4
  in
  let month = if accept '-' then num 2 else 1 in
  let day = if accept '-' then num 2 else 1 in
  let h, mi, sec, ms =
    if accept 'T' || accept ' ' then begin
      let h = num 2 in
      if not (accept ':') then fail ();
      let mi = num 2 in
      let sec = if accept ':' then num 2 else 0 in
      let ms =
        if accept '.' then begin
          let start = !pos in
          while !pos < len && s.[!pos] >= '0' && s.[!pos] <= '9' do
            incr pos
          done;
          let frac = String.sub s start (!pos - start) in
          if frac = "" then fail ();
          int_of_string (String.sub (frac ^ "00") 0 3)
        end
        else 0
      in
      (h, mi, sec, ms)
    end
    else (0, 0, 0, 0)
  in
  let offset =
    if accept 'Z' then 0
    else if !pos < len && (s.[!pos] = '+' || s.[!pos] = '-') then begin
      let sign = if s.[!pos] = '-' then -1 else 1 in
      incr pos;
      let oh = num 2 in
      ignore (accept ':');
      let om = num 2 in
      sign * ((oh * 60) + om)
    end
    else 0
  in
  if !pos <> len then fail ();
  utc ~hours:h ~minutes:mi ~seconds:sec ~ms year (month - 1) day
  -. float_of_int (offset * 60000)
