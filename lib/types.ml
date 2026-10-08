(** Option types shared by several modules (src/utils/types.ts). *)

type range = [ `N of int | `Range of int * int ]
(** A fixed number or an inclusive [min, max] range ([NumberOrRange]). *)

type casing = [ `Upper | `Lower | `Mixed ]
type sex = [ `Female | `Male ]
