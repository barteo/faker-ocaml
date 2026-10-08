(* Shortcuts for building JSON results in parity cases. *)
module J = Faker.Json

let s x = J.Str x
let n x = J.Num x
let i x = J.Num (float_of_int x)
let b x = J.Bool x
let ss xs = J.Arr (Array.map s xs)
let is xs = J.Arr (Array.map i xs)
let arr f xs = J.Arr (Array.map f xs)
let lst f xs = J.Arr (Array.of_list (List.map f xs))
let opt f = function None -> J.Null | Some x -> f x
let date ms = J.Str (Faker.Date_util.to_iso ms)

type case = string * (Faker.t -> J.t)
