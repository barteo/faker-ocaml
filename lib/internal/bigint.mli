(** Arbitrary-precision integers with JavaScript [BigInt] semantics: the type of
    [number.bigInt]'s options and result. *)

type t

val zero : t
val one : t
val of_int : int -> t

val to_int_opt : t -> int option
(** [None] when the value doesn't fit in a native [int]. *)

val to_string : t -> string
(** Base 10, like JS [toString()]. *)

val of_string : string -> t
(** JS [BigInt(string)]: surrounding whitespace is ignored, [""] is [0], a sign
    is only allowed on decimals and [0x]/[0o]/[0b] select a radix. Raises
    [Faker_error "Cannot convert <s> to a BigInt"] otherwise. *)

val of_float : float -> t
(** JS [BigInt(number)]: exact for every integral number. Raises [Faker_error]
    with V8's message for a non-integer, [NaN] or an infinity. *)

val of_bool : bool -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

val div : t -> t -> t
(** Truncates toward zero, like JS [/]. Raises [Division_by_zero]. *)

val rem : t -> t -> t
(** Takes the sign of the dividend, like JS [%]. Raises [Division_by_zero]. *)
