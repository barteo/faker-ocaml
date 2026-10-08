(* Port of faker-js src/core.ts: the state shared by every module. *)

exception Faker_error of string

let error fmt = Printf.ksprintf (fun s -> raise (Faker_error s)) fmt

type t = {
  randomizer : Randomizer.t;
  locale : Json.t;  (** Merged locale definitions (an [Obj] of categories). *)
  mutable default_ref_date : unit -> float;  (** Epoch milliseconds. *)
}

let now () = Float.round (Unix.gettimeofday () *. 1000.0)

(* Port of src/utils/merge-locales.ts: earlier locales take precedence per entry. *)
let merge_locales (locales : Json.t list) : Json.t =
  let merged : (string * (string * Json.t) list) list ref = ref [] in
  List.iter
    (function
      | Json.Obj cats ->
          List.iter
            (fun (cat, value) ->
              let entries = match value with Json.Obj e -> e | _ -> [] in
              match List.assoc_opt cat !merged with
              | None -> merged := !merged @ [ (cat, entries) ]
              | Some existing ->
                  (* { ...value, ...existing }: keys of [value] first, existing values win *)
                  let combined =
                    List.map
                      (fun (k, v) ->
                        match List.assoc_opt k existing with
                        | Some e -> (k, e)
                        | None -> (k, v))
                      entries
                    @ List.filter
                        (fun (k, _) -> not (List.mem_assoc k entries))
                        existing
                  in
                  merged :=
                    List.map
                      (fun (c, e) -> if c = cat then (c, combined) else (c, e))
                      !merged)
            cats
      | _ -> ())
    locales;
  Json.Obj (List.map (fun (c, e) -> (c, Json.Obj e)) !merged)

let create ?(locale = [ Json.Obj [] ]) ?randomizer ?seed () =
  let randomizer =
    match randomizer with Some r -> r | None -> Randomizer.mersenne53 ()
  in
  Option.iter (fun s -> randomizer.seed (`Int s)) seed;
  { randomizer; locale = merge_locales locale; default_ref_date = now }

let next f = f.randomizer.next ()
let ref_date f = f.default_ref_date ()
