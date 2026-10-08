(* Port of src/modules/date/module.ts (SimpleDateModule and DateModule) and
   src/internal/date.ts. Dates are float epoch milliseconds. *)

(* ---------- JS Date emulation ---------- *)

type input = [ `Num of float | `Str of string | `Date of float ]
(** A date argument as upstream receives it ([string | Date | number]). [`Date]
    is an already constructed Date object (whose time value may be NaN). *)

let ms_per_day = 86400000.0

(* TimeClip *)
let time_clip t =
  if Float.is_nan t || Float.abs t > 8.64e15 then Float.nan
  else Float.trunc t +. 0.0

(* MakeDay(year, month, date) *)
let make_day year month date =
  if not (Float.is_finite year && Float.is_finite month && Float.is_finite date)
  then Float.nan
  else
    let y = Float.trunc year
    and m = Float.trunc month
    and dt = Float.trunc date in
    let ym = y +. Float.floor (m /. 12.0) in
    if Float.abs ym > 1e6 then Float.nan
    else
      let mn = int_of_float (Float.rem (Float.rem m 12.0 +. 12.0) 12.0) in
      float_of_int (Date_util.days_from_civil (int_of_float ym) (mn + 1) 1)
      +. dt -. 1.0

(* MakeDate(day, time) *)
let make_date day time = Js.mul day ms_per_day +. time

let time_within_day t =
  let r = Float.rem t ms_per_day in
  if r < 0.0 then r +. ms_per_day else r

(** [Date.prototype.setUTCFullYear(y)]: keeps month, day and time (with day
    overflow). *)
let set_utc_full_year (t : float) (year : float) : float =
  let t = if Float.is_nan t then 0.0 else t in
  let p = Date_util.to_parts t in
  time_clip
    (make_date
       (make_day year (float_of_int p.month) (float_of_int p.day))
       (time_within_day t))

(** [Date.prototype.setUTCDate(d)] *)
let set_utc_date (t : float) (date : float) : float =
  if Float.is_nan t then Float.nan
  else
    let p = Date_util.to_parts t in
    time_clip
      (make_date
         (make_day (float_of_int p.year) (float_of_int p.month) date)
         (time_within_day t))

let utc_full_year (t : float) = (Date_util.to_parts t).year

(** [new Date(x)] *)
let new_date (d : input) : float =
  match d with
  | `Date t -> t
  | `Num n -> time_clip n
  | `Str s -> (
      try time_clip (Date_util.of_iso s) with Invalid_argument _ -> Float.nan)

let input_to_string (d : input) =
  match d with
  | `Date _ -> "Invalid Date"
  | `Num n -> Js.number_to_string n
  | `Str s -> s

(** Port of [toDate] (src/internal/date.ts). *)
let to_date ?(name = "refDate") (d : input) : float =
  let t = new_date d in
  if Float.is_nan t then
    Core.error "Invalid %s date: %s" name (input_to_string d);
  t

let ref_input ref_date f : input =
  match ref_date with Some d -> d | None -> `Num (Core.ref_date f)

(* ---------- SimpleDateModule ---------- *)

let between_input ~(from : input) ~(to_ : input) f : float =
  let from_ms = to_date ~name:"from" from in
  let to_ms = to_date ~name:"to" to_ in
  if from_ms > to_ms then Core.error "`from` date must be before `to` date.";
  Fk_number.int_f ~min:from_ms ~max:to_ms f

let anytime_input ?ref_date f =
  let ref_date = ref_input ref_date f in
  let time = to_date ref_date in
  let year_ms = 1000.0 *. 60.0 *. 60.0 *. 24.0 *. 365.0 in
  between_input ~from:(`Num (time -. year_ms)) ~to_:(`Num (time +. year_ms)) f

let years_range (years : Types.range) =
  let min, max = match years with `N n -> (0, n) | `Range (a, b) -> (a, b) in
  if max <= 0 then Core.error "Years must be greater than 0.";
  if min >= max then
    Core.error
      "The maximum amount of years must be greater than the minimum amount of \
       years.";
  (float_of_int min, float_of_int max)

let days_range (days : Types.range) =
  let min, max = match days with `N n -> (0, n) | `Range (a, b) -> (a, b) in
  if max <= 0 then Core.error "Days must be greater than 0.";
  if min >= max then
    Core.error
      "The maximum amount of days must be greater than the minimum amount of \
       days.";
  (float_of_int min, float_of_int max)

let past_input ?(years = `N 1) ?ref_date f =
  let ref_date = ref_input ref_date f in
  let min, max = years_range years in
  let time = to_date ref_date in
  let from =
    set_utc_full_year time (float_of_int (utc_full_year time) -. max)
  in
  let to_ = set_utc_full_year time (float_of_int (utc_full_year time) -. min) in
  between_input ~from:(`Date from) ~to_:(`Num (to_ -. 1000.0)) f

let future_input ?(years = `N 1) ?ref_date f =
  let ref_date = ref_input ref_date f in
  let min, max = years_range years in
  let time = to_date ref_date in
  let from =
    set_utc_full_year time (float_of_int (utc_full_year time) +. min)
  in
  let to_ = set_utc_full_year time (float_of_int (utc_full_year time) +. max) in
  between_input ~from:(`Num (from +. 1000.0)) ~to_:(`Date to_) f

let betweens_input ?(count = `N 3) ~(from : input) ~(to_ : input) f :
    float array =
  let dates =
    Fk_helpers.multiple ~count (fun _ -> between_input ~from ~to_ f) f
  in
  let l = List.stable_sort (fun a b -> compare a b) (Array.to_list dates) in
  Array.of_list l

let recent_input ?(days = `N 1) ?ref_date f =
  let ref_date = ref_input ref_date f in
  let min, max = days_range days in
  let time = to_date ref_date in
  let day = float_of_int (Date_util.to_parts time).day in
  let from = set_utc_date time (day -. max) in
  let to_ = set_utc_date time (day -. min) in
  between_input ~from:(`Date from) ~to_:(`Num (to_ -. 1000.0)) f

let soon_input ?(days = `N 1) ?ref_date f =
  let ref_date = ref_input ref_date f in
  let min, max = days_range days in
  let time = to_date ref_date in
  let day = float_of_int (Date_util.to_parts time).day in
  let from = set_utc_date time (day +. min) in
  let to_ = set_utc_date time (day +. max) in
  between_input ~from:(`Num (from +. 1000.0)) ~to_:(`Date to_) f

type birthdate_mode = [ `Age | `Year ]

let birthdate_input ?(mode : birthdate_mode = `Age) ?(min = 18) ?(max = 80)
    ?ref_date f =
  let ref_date = ref_input ref_date f in
  let ref_date = to_date ref_date in
  let ref_year = float_of_int (utc_full_year ref_date) in
  match mode with
  | `Age ->
      let one_day = 24.0 *. 60.0 *. 60.0 *. 1000.0 in
      let from =
        set_utc_full_year ref_date (ref_year -. float_of_int max -. 1.0)
        +. one_day
      in
      let to_ = set_utc_full_year ref_date (ref_year -. float_of_int min) in
      if from > to_ then
        Core.error "Max age %d should be greater than or equal to min age %d."
          max min;
      between_input ~from:(`Num from) ~to_:(`Num to_) f
  | `Year ->
      let from =
        set_utc_full_year (Date_util.utc 1900 0 2) (float_of_int min)
      in
      let to_ =
        set_utc_full_year (Date_util.utc 1900 11 30) (float_of_int max)
      in
      if from > to_ then
        Core.error "Max year %d should be greater than or equal to min year %d."
          max min;
      between_input ~from:(`Num from) ~to_:(`Num to_) f

(* ---------- public API (dates as float epoch milliseconds) ---------- *)

let num_opt = Option.map (fun d -> `Num d)

(** A random date within one year before or after [ref_date]. *)
let anytime ?ref_date f = anytime_input ?ref_date:(num_opt ref_date) f

(** A random date in the past [years] (default 1) before [ref_date]. *)
let past ?years ?ref_date f = past_input ?years ?ref_date:(num_opt ref_date) f

(** A random date in the next [years] (default 1) after [ref_date]. *)
let future ?years ?ref_date f =
  future_input ?years ?ref_date:(num_opt ref_date) f

(** A random date between [from] and [to_] (inclusive). *)
let between ~from ~to_ f = between_input ~from:(`Num from) ~to_:(`Num to_) f

(** [count] (default 3) random dates between [from] and [to_], sorted ascending.
*)
let betweens ?count ~from ~to_ f =
  betweens_input ?count ~from:(`Num from) ~to_:(`Num to_) f

(** A random date in the past [days] (default 1) before [ref_date]. *)
let recent ?days ?ref_date f = recent_input ?days ?ref_date:(num_opt ref_date) f

(** A random date in the next [days] (default 1) after [ref_date]. *)
let soon ?days ?ref_date f = soon_input ?days ?ref_date:(num_opt ref_date) f

(** A random birthdate: by age (default 18-80) or by year range ([~mode:`Year]).
*)
let birthdate ?mode ?min ?max ?ref_date f =
  birthdate_input ?mode ?min ?max ?ref_date:(num_opt ref_date) f

(* ---------- DateModule ---------- *)

let localized_entry entry ~abbreviated ~context f =
  let source = Locale.get f "date" entry in
  let present k =
    match Json.member k source with
    | Some Json.Null | None -> false
    | Some _ -> true
  in
  let typ =
    if abbreviated then
      if context && present "abbr_context" then "abbr_context" else "abbr"
    else if context && present "wide_context" then "wide_context"
    else "wide"
  in
  let path = "date." ^ entry ^ "." ^ typ in
  match Json.member typ source with
  | Some Json.Null -> Locale.not_applicable path
  | None -> Locale.missing path
  | Some values -> Fk_helpers.array_element (Locale.to_strings values) f

(** A random month name. *)
let month ?(abbreviated = false) ?(context = false) f =
  localized_entry "month" ~abbreviated ~context f

(** A random weekday name. *)
let weekday ?(abbreviated = false) ?(context = false) f =
  localized_entry "weekday" ~abbreviated ~context f

(** A random IANA time zone name. *)
let time_zone f =
  Fk_helpers.array_element (Locale.strings f "date" "time_zone") f

(* ---------- registry ---------- *)

let date_arg (o : Args.opts) key : input option =
  match List.assoc_opt key o with
  | None -> None
  | Some (Json.Num n) -> Some (`Num n)
  | Some (Json.Str s) -> Some (`Str s)
  | Some Json.Null -> Some (`Num 0.0) (* new Date(null) is the epoch *)
  | Some (Json.Bool b) -> Some (`Num (if b then 1.0 else 0.0))
  | Some (Json.Arr _ | Json.Obj _) -> Some (`Num Float.nan)

let ref_arg (o : Args.opts) = date_arg o "refDate"

let required_date (o : Args.opts) key : input =
  match date_arg o key with
  | Some d -> d
  | None ->
      Core.error "Cannot read properties of undefined (reading 'toString')"

let iso t = Json.Str (Date_util.to_iso t)

let registry : (string * Registry.fn) list =
  let open Args in
  [
    ( "anytime",
      fun f a ->
        let o = opts a in
        iso (anytime_input ?ref_date:(ref_arg o) f) );
    ( "past",
      fun f a ->
        let o = opts a in
        iso (past_input ?years:(range o "years") ?ref_date:(ref_arg o) f) );
    ( "future",
      fun f a ->
        let o = opts a in
        iso (future_input ?years:(range o "years") ?ref_date:(ref_arg o) f) );
    ( "between",
      fun f a ->
        let o = opts a in
        iso
          (between_input ~from:(required_date o "from")
             ~to_:(required_date o "to") f) );
    ( "betweens",
      fun f a ->
        let o = opts a in
        let from = required_date o "from" and to_ = required_date o "to" in
        Json.Arr
          (Array.map iso (betweens_input ?count:(range o "count") ~from ~to_ f))
    );
    ( "recent",
      fun f a ->
        let o = opts a in
        iso (recent_input ?days:(range o "days") ?ref_date:(ref_arg o) f) );
    ( "soon",
      fun f a ->
        let o = opts a in
        iso (soon_input ?days:(range o "days") ?ref_date:(ref_arg o) f) );
    ( "birthdate",
      fun f a ->
        let o = opts a in
        let mode =
          match string o "mode" with
          | None -> Some `Age
          | Some "age" -> Some `Age
          | Some "year" -> Some `Year
          | Some _ -> None
        in
        match mode with
        | None ->
            (* unknown mode: upstream validates refDate, then returns undefined *)
            ignore (to_date (ref_input (ref_arg o) f));
            Json.Null
        | Some mode ->
            iso
              (birthdate_input ~mode ?min:(int o "min") ?max:(int o "max")
                 ?ref_date:(ref_arg o) f) );
    ( "month",
      fun f a ->
        let o = opts a in
        str
          (month ?abbreviated:(bool o "abbreviated") ?context:(bool o "context")
             f) );
    ( "weekday",
      fun f a ->
        let o = opts a in
        str
          (weekday ?abbreviated:(bool o "abbreviated")
             ?context:(bool o "context") f) );
    ("timeZone", fun f _ -> str (time_zone f));
  ]
