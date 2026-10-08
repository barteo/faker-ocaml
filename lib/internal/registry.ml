(* Method registry used by helpers.fake() to resolve "{{module.method}}"
   expressions. Keys are the upstream (camelCase) names. *)

type fn = Core.t -> Json.t list -> Json.t

let modules : (string, (string, fn) Hashtbl.t) Hashtbl.t = Hashtbl.create 32

let add (module_name : string) (methods : (string * fn) list) =
  let tbl =
    match Hashtbl.find_opt modules module_name with
    | Some t -> t
    | None ->
        let t = Hashtbl.create 32 in
        Hashtbl.replace modules module_name t;
        t
  in
  List.iter (fun (name, fn) -> Hashtbl.replace tbl name fn) methods

let find_module name = Hashtbl.find_opt modules name
let find_method m name = Option.bind (find_module m) (fun t -> Hashtbl.find_opt t name)
