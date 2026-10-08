open Faker

let seq gen seed =
  let r : Randomizer.t = gen ?seed:None () in
  r.seed seed;
  Json.Arr (Array.init 1000 (fun _ -> Json.Num (r.next ())))

let seeds = [ 0; 1; 42; 1337; -1; 1 lsl 31; (1 lsl 32) + 5; 123456789012 ]

let cases : (string * (Faker.t -> Json.t)) list =
  List.map
    (fun s ->
      (Printf.sprintf "f53/%d" s, fun _ -> seq Randomizer.mersenne53 (`Int s)))
    seeds
  @ List.map
      (fun s ->
        (Printf.sprintf "f32/%d" s, fun _ -> seq Randomizer.mersenne32 (`Int s)))
      seeds
  @ [
      ("f53/[1,2,3]", fun _ -> seq Randomizer.mersenne53 (`Array [| 1; 2; 3 |]));
      ("f53/[42]", fun _ -> seq Randomizer.mersenne53 (`Array [| 42 |]));
      ( "f32/[5,4294967295,7,8,9]",
        fun _ -> seq Randomizer.mersenne32 (`Array [| 5; 4294967295; 7; 8; 9 |])
      );
    ]
