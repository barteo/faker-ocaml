(* Registry entries for the helpers module. *)

open Args

let arr = function Some (Json.Arr a) -> a | _ -> [||]

let registry : (string * Registry.fn) list =
  [
    ("arrayElement", fun f a -> Fk_helpers.array_element (arr (nth a 0)) f);
    ( "arrayElements",
      fun f a ->
        let count = match nth a 1 with Some (Json.Num n) -> Some (`N (int_of_float n)) | Some r -> range [ ("c", r) ] "c" | None -> None in
        Json.Arr (Fk_helpers.array_elements ?count (arr (nth a 0)) f) );
    ("shuffle", fun f a -> Json.Arr (Fk_helpers.shuffle (arr (nth a 0)) f));
    ( "weightedArrayElement",
      fun f a ->
        Fk_helpers.weighted_array_element
          (Array.map
             (fun e ->
               ( (match Json.member "weight" e with Some (Json.Num w) -> w | _ -> 0.0),
                 Option.value ~default:Json.Null (Json.member "value" e) ))
             (arr (nth a 0)))
          f );
    ( "uniqueArray",
      fun f a ->
        let n = match nth a 1 with Some (Json.Num n) -> int_of_float n | _ -> 0 in
        Json.Arr (Fk_helpers.unique_array (arr (nth a 0)) n f) );
    ( "objectKey",
      fun f a -> match nth a 0 with Some (Json.Obj o) -> str (Fk_helpers.object_key o f) | _ -> Json.Null );
    ( "objectValue",
      fun f a -> match nth a 0 with Some (Json.Obj o) -> Fk_helpers.object_value o f | _ -> Json.Null );
    ( "objectEntry",
      fun f a ->
        match nth a 0 with
        | Some (Json.Obj o) ->
            let k, v = Fk_helpers.object_entry o f in
            Json.Arr [| str k; v |]
        | _ -> Json.Null );
    ( "enumValue",
      fun f a -> match nth a 0 with Some (Json.Obj o) -> Fk_helpers.enum_value o f | _ -> Json.Null );
    ( "rangeToNumber",
      fun f a ->
        match nth a 0 with
        | Some r -> (
            match range [ ("r", r) ] "r" with Some r -> int_ (Fk_helpers.range_to_number r f) | None -> Json.Null)
        | None -> Json.Null );
    ("slugify", fun _ a -> match nth a 0 with Some (Json.Str s) -> str (Fk_helpers.slugify s) | _ -> str "");
    ( "replaceSymbols",
      fun f a -> match nth a 0 with Some (Json.Str s) -> str (Fk_helpers.replace_symbols s f) | _ -> str "" );
    ( "replaceCreditCardSymbols",
      fun f a ->
        let s = match nth a 0 with Some (Json.Str s) -> s | _ -> "6453-####-####-####-###L" in
        let symbol = match nth a 1 with Some (Json.Str s) when s <> "" -> Some s.[0] | _ -> None in
        str (Fk_helpers.replace_credit_card_symbols ?symbol s f) );
    ( "fromRegExp",
      fun f a -> match nth a 0 with Some (Json.Str s) -> str (Fk_helpers_regexp.from_reg_exp s f) | _ -> str "" );
    ( "fake",
      fun f a ->
        match nth a 0 with
        | Some (Json.Str s) -> str (Fake.fake s f)
        | Some (Json.Arr p) -> str (Fake.fake_one_of (Array.map Json.to_js_string p) f)
        | _ -> str "" );
    ( "mustache",
      fun _ a ->
        let text = match nth a 0 with Some (Json.Str s) -> Some s | _ -> None in
        let data = match nth a 1 with Some (Json.Obj o) -> List.map (fun (k, v) -> (k, `S (Json.to_js_string v))) o | _ -> [] in
        str (Fake.mustache text data) );
    ( "multiple",
      fun _ _ -> Core.error "helpers.multiple cannot be used from a template" );
  ]
