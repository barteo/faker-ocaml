(* Port of src/modules/number/module.ts. *)

let num = Js.number_to_string
let max_safe_integer = 9007199254740991.0

(* number.int on JS doubles; [int] below is the integer-typed public API. *)
let int_f ?(min = 0.0) ?(max = max_safe_integer) ?(multiple_of = 1.0)
    ?(distributor = Distributor.uniform) (f : Core.t) : float =
  if
    not
      (Float.is_integer multiple_of && Float.abs multiple_of <= max_safe_integer)
  then Core.error "multipleOf should be an integer.";
  if multiple_of <= 0.0 then Core.error "multipleOf should be greater than 0.";
  let effective_min = Float.ceil (min /. multiple_of) in
  let effective_max = Float.floor (max /. multiple_of) in
  if effective_min = effective_max then effective_min *. multiple_of
  else if effective_max < effective_min then
    if max >= min then
      Core.error "No suitable integer value between %s and %s found." (num min)
        (num max)
    else Core.error "Max %s should be greater than min %s." (num max) (num min)
  else
    let real = distributor f.randomizer in
    let delta = effective_max -. effective_min +. 1.0 in
    Float.floor (Js.mul real delta +. effective_min) *. multiple_of

let int ?min ?max ?multiple_of ?distributor f =
  int_of_float
    (int_f
       ?min:(Option.map float_of_int min)
       ?max:(Option.map float_of_int max)
       ?multiple_of:(Option.map float_of_int multiple_of)
       ?distributor f)

let float ?(min = 0.0) ?(max = 1.0) ?fraction_digits ?multiple_of
    ?(distributor = Distributor.uniform) f =
  if max < min then
    Core.error "Max %s should be greater than min %s." (num max) (num min);
  let effective_multiple_of =
    match (multiple_of, fraction_digits) with
    | Some m, _ -> Some m
    | None, Some d -> Some (Float.pow 10.0 (-.float_of_int d))
    | None, None -> None
  in
  (match fraction_digits with
  | Some d ->
      if multiple_of <> None then
        Core.error
          "multipleOf and fractionDigits cannot be set at the same time.";
      if d < 0 then
        Core.error "fractionDigits should be greater than or equal to 0."
  | None -> ());
  match effective_multiple_of with
  | Some multiple_of ->
      if multiple_of <= 0.0 then
        Core.error "multipleOf should be greater than 0.";
      let log_precision = Float.log10 multiple_of in
      let factor =
        if multiple_of < 1.0 && Float.is_integer log_precision then
          Float.pow 10.0 (-.log_precision)
        else 1.0 /. multiple_of
      in
      let i = int_f ~min:(min *. factor) ~max:(max *. factor) ~distributor f in
      i /. factor
  | None ->
      let real = distributor f.randomizer in
      Js.mul real (max -. min) +. min

let in_radix radix default_max ?(min = 0) ?(max = default_max) f =
  Js.int_to_radix (int ~min ~max f) radix

let binary ?min ?max f = in_radix 2 1 ?min ?max f
let octal ?min ?max f = in_radix 8 7 ?min ?max f
let hex ?min ?max f = in_radix 16 15 ?min ?max f

let roman_numeral ?(min = 1) ?(max = 3999) f =
  if min < 1 then Core.error "Min value %d should be 1 or greater." min;
  if max > 3999 then Core.error "Max value %d should be 3999 or less." max;
  let n = ref (int ~min ~max f) in
  let lookup =
    [
      ("M", 1000);
      ("CM", 900);
      ("D", 500);
      ("CD", 400);
      ("C", 100);
      ("XC", 90);
      ("L", 50);
      ("XL", 40);
      ("X", 10);
      ("IX", 9);
      ("V", 5);
      ("IV", 4);
      ("I", 1);
    ]
  in
  let b = Buffer.create 16 in
  List.iter
    (fun (k, v) ->
      Buffer.add_string b (Js.repeat k (!n / v));
      n := !n mod v)
    lookup;
  Buffer.contents b

let registry : (string * Registry.fn) list =
  [
    ( "int",
      fun f a ->
        let o = Args.opts ~shorthand:"max" a in
        Args.num
          (int_f ?min:(Args.float o "min") ?max:(Args.float o "max")
             ?multiple_of:(Args.float o "multipleOf")
             f) );
    ( "float",
      fun f a ->
        let o = Args.opts ~shorthand:"max" a in
        Args.num
          (float ?min:(Args.float o "min") ?max:(Args.float o "max")
             ?fraction_digits:(Args.int o "fractionDigits")
             ?multiple_of:(Args.float o "multipleOf")
             f) );
    ( "binary",
      fun f a ->
        let o = Args.opts ~shorthand:"max" a in
        Args.str (binary ?min:(Args.int o "min") ?max:(Args.int o "max") f) );
    ( "octal",
      fun f a ->
        let o = Args.opts ~shorthand:"max" a in
        Args.str (octal ?min:(Args.int o "min") ?max:(Args.int o "max") f) );
    ( "hex",
      fun f a ->
        let o = Args.opts ~shorthand:"max" a in
        Args.str (hex ?min:(Args.int o "min") ?max:(Args.int o "max") f) );
    ( "romanNumeral",
      fun f a ->
        let o = Args.opts ~shorthand:"max" a in
        Args.str
          (roman_numeral ?min:(Args.int o "min") ?max:(Args.int o "max") f) );
  ]
