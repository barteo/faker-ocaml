(* Port of src/modules/finance/module.ts, bitcoin.ts and iban.ts (data in
   Fk_finance_iban). *)

type currency = { name : string; code : string; symbol : string; numeric_code : string }

type bitcoin_address_family = [ `Legacy | `Segwit | `Bech32 | `Taproot ]
type bitcoin_network = [ `Mainnet | `Testnet ]

let pretty_print_iban (iban : string) : string =
  let b = Buffer.create (String.length iban + 8) in
  let i = ref 0 in
  while !i < String.length iban do
    Buffer.add_string b (Js.substring iban !i (!i + 4));
    Buffer.add_char b ' ';
    i := !i + 4
  done;
  (* trimEnd *)
  let s = Buffer.contents b in
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let pick entry f = Fk_helpers.array_element (Locale.strings f "finance" entry) f

let account_number ?(length = 8) f =
  Fk_string.numeric ~length:(`N length) ~allow_leading_zeros:true f

let account_name f = String.concat " " [ pick "account_type" f; "Account" ]

let routing_number f =
  let federal_reserve_routing_symbol = pick "federal_reserve_routing_symbol" f in
  let institution_identifier = Fk_string.numeric ~length:(`N 4) ~allow_leading_zeros:true f in
  let routing_number = federal_reserve_routing_symbol ^ institution_identifier in
  let len = String.length routing_number in
  (* Number(undefined) is NaN; [Number(x) || 0] maps it to 0. *)
  let digit i = if i < len then Char.code routing_number.[i] - 48 else 0 in
  let sum = ref 0 in
  let i = ref 0 in
  while !i < len do
    sum := !sum + (digit !i * 3);
    sum := !sum + (digit (!i + 1) * 7);
    sum := !sum + digit (!i + 2);
    i := !i + 3
  done;
  let check = (int_of_float (Float.ceil (float_of_int !sum /. 10.0)) * 10) - !sum in
  routing_number ^ string_of_int check

(* Number.prototype.toLocaleString('en-US', { minimumFractionDigits: dec }) for a value
   that already has at most [dec] fraction digits. *)
let to_locale_string_en (value : float) (dec : int) : string =
  let s = Js.to_fixed value dec in
  let neg = String.length s > 0 && s.[0] = '-' in
  let s = if neg then String.sub s 1 (String.length s - 1) else s in
  let ip, fp =
    match String.index_opt s '.' with
    | Some i -> (String.sub s 0 i, String.sub s i (String.length s - i))
    | None -> (s, "")
  in
  let b = Buffer.create (String.length s + 8) in
  let l = String.length ip in
  String.iteri
    (fun i c ->
      if i > 0 && (l - i) mod 3 = 0 then Buffer.add_char b ',';
      Buffer.add_char b c)
    ip;
  (if neg then "-" else "") ^ Buffer.contents b ^ fp

let amount ?(min = 0.0) ?(max = 1000.0) ?(dec = 2) ?(symbol = "") ?(auto_format = false) f =
  let rand_value = Fk_number.float ~max ~min ~fraction_digits:dec f in
  let formatted =
    if auto_format then to_locale_string_en rand_value dec else Js.to_fixed rand_value dec
  in
  symbol ^ formatted

let transaction_type f = pick "transaction_type" f

let currency_of_json (j : Json.t) : currency =
  let field k = match Json.member k j with Some (Json.Str s) -> s | _ -> "" in
  { name = field "name"; code = field "code"; symbol = field "symbol"; numeric_code = field "numericCode" }

let currency f : currency =
  let data = match Locale.get f "finance" "currency" with Json.Arr a -> a | v -> [| v |] in
  currency_of_json (Fk_helpers.array_element data f)

let currency_code f = (currency f).code
let currency_name f = (currency f).name

let currency_symbol f =
  let rec go () =
    let symbol = (currency f).symbol in
    if symbol = "" then go () else symbol
  in
  go ()

let currency_numeric_code f = (currency f).numeric_code

(* ---------- bitcoin.ts ---------- *)

let bitcoin_families : bitcoin_address_family array = [| `Legacy; `Segwit; `Bech32; `Taproot |]

(* prefix (mainnet, testnet), length range, casing, exclude *)
let bitcoin_address_spec : bitcoin_address_family -> (string * string) * (int * int) * Types.casing * string
    = function
  | `Legacy -> (("1", "m"), (26, 34), `Mixed, "0OIl")
  | `Segwit -> (("3", "2"), (26, 34), `Mixed, "0OIl")
  | `Bech32 -> (("bc1", "tb1"), (42, 42), `Lower, "1bBiIoO")
  | `Taproot -> (("bc1p", "tb1p"), (62, 62), `Lower, "1bBiIoO")

let bitcoin_address ?type_ ?(network = `Mainnet) f =
  let type_ = match type_ with Some t -> t | None -> Fk_helpers.array_element bitcoin_families f in
  let (mainnet, testnet), (min, max), casing, exclude = bitcoin_address_spec type_ in
  let address_prefix = match network with `Mainnet -> mainnet | `Testnet -> testnet in
  let address_length = Fk_number.int ~min ~max f in
  let address =
    Fk_string.alphanumeric
      ~length:(`N (address_length - String.length address_prefix))
      ~casing ~exclude:(Js.code_points exclude) f
  in
  address_prefix ^ address

let litecoin_address f =
  let address_length = Fk_number.int ~min:26 ~max:33 f in
  let first = Fk_string.from_characters "LM3" f in
  let rest =
    Fk_string.from_characters ~length:(`N (address_length - 1))
      "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ" f
  in
  first ^ rest

(* ---------- credit cards ---------- *)

let to_array = function Json.Arr a -> a | v -> [| v |]

let credit_card_data f = match Locale.get f "finance" "credit_card" with Json.Obj o -> o | _ -> []

(** [credit_card_number ?issuer f]: [issuer] is an issuer name (case-insensitive) or a
    custom format containing ['#']. *)
let credit_card_number ?(issuer = "") f =
  let local_format = credit_card_data f in
  let normalized_issuer = String.lowercase_ascii issuer in
  let format =
    match List.assoc_opt normalized_issuer local_format with
    | Some formats -> Locale.to_string (Fk_helpers.array_element (to_array formats) f)
    | None ->
        if String.contains issuer '#' then issuer
        else
          let formats = Fk_helpers.object_value local_format f in
          Locale.to_string (Fk_helpers.array_element (to_array formats) f)
  in
  let format = Js.replace_all ~sub:"/" ~by:"" format in
  Fk_helpers.replace_credit_card_symbols format f

let credit_card_cvv f = Fk_string.numeric ~length:(`N 3) ~allow_leading_zeros:true f
let credit_card_issuer f = Fk_helpers.object_key (credit_card_data f) f

let pin ?(length = 4) f =
  if length < 1 then Core.error "minimum length is 1";
  Fk_string.numeric ~length:(`N length) ~allow_leading_zeros:true f

let ethereum_address f = Fk_string.hexadecimal ~length:(`N 40) ~casing:`Lower f

(* ---------- iban / bic ---------- *)

let iban ?(formatted = false) ?country_code f =
  let module I = Fk_finance_iban in
  let iban_format =
    match country_code with
    | Some c when c <> "" -> (
        match Array.find_opt (fun (x : I.format) -> x.country = c) I.formats with
        | Some x -> x
        | None -> Core.error "Country code %s not supported." c)
    | _ -> Fk_helpers.array_element I.formats f
  in
  let s = ref "" in
  let count = ref 0 in
  Array.iter
    (fun (bban : I.bban) ->
      let c = ref bban.count in
      count := !count + bban.count;
      while !c > 0 do
        (match bban.type_ with
        | 'a' -> s := !s ^ Fk_helpers.array_element I.alpha f
        | 'c' ->
            if Fk_datatype.boolean ~probability:0.8 f then
              s := !s ^ string_of_int (Fk_number.int ~max:9 f)
            else s := !s ^ Fk_helpers.array_element I.alpha f
        | _ ->
            if !c >= 3 && Fk_datatype.boolean ~probability:0.3 f then
              if Fk_datatype.boolean f then begin
                s := !s ^ Fk_helpers.array_element I.pattern100 f;
                c := !c - 2
              end
              else begin
                s := !s ^ Fk_helpers.array_element I.pattern10 f;
                decr c
              end
            else s := !s ^ string_of_int (Fk_number.int ~max:9 f));
        decr c
      done;
      s := Js.substring !s 0 !count)
    iban_format.bban;
  let checksum = 98 - I.mod97 (I.to_digit_string (!s ^ iban_format.country ^ "00")) in
  let checksum = if checksum < 10 then "0" ^ string_of_int checksum else string_of_int checksum in
  let result = iban_format.country ^ checksum ^ !s in
  if formatted then pretty_print_iban result else result

let bic ?include_branch_code f =
  let include_branch_code =
    match include_branch_code with Some b -> b | None -> Fk_datatype.boolean f
  in
  let bank_identifier = Fk_string.alpha ~length:(`N 4) ~casing:`Upper f in
  let country_code = Fk_helpers.array_element Fk_finance_iban.iso3166 f in
  let location_code = Fk_string.alphanumeric ~length:(`N 2) ~casing:`Upper f in
  let branch_code =
    if include_branch_code then
      if Fk_datatype.boolean f then Fk_string.alphanumeric ~length:(`N 3) ~casing:`Upper f
      else "XXX"
    else ""
  in
  bank_identifier ^ country_code ^ location_code ^ branch_code

let transaction_description f =
  Fake.fake_json (Locale.get f "finance" "transaction_description_pattern") f

(* ---------- registry ---------- *)

let currency_to_json (c : currency) : Json.t =
  Json.Obj
    [ ("name", Json.Str c.name); ("code", Json.Str c.code); ("symbol", Json.Str c.symbol);
      ("numericCode", Json.Str c.numeric_code) ]

let registry : (string * Registry.fn) list =
  let open Args in
  [
    ( "accountNumber",
      fun f a -> let o = opts ~shorthand:"length" a in str (account_number ?length:(int o "length") f) );
    ("accountName", fun f _ -> str (account_name f));
    ("routingNumber", fun f _ -> str (routing_number f));
    ( "amount",
      fun f a ->
        let o = opts a in
        str
          (amount ?min:(float o "min") ?max:(float o "max") ?dec:(int o "dec")
             ?symbol:(string o "symbol") ?auto_format:(bool o "autoFormat") f) );
    ("transactionType", fun f _ -> str (transaction_type f));
    ("currency", fun f _ -> currency_to_json (currency f));
    ("currencyCode", fun f _ -> str (currency_code f));
    ("currencyName", fun f _ -> str (currency_name f));
    ("currencySymbol", fun f _ -> str (currency_symbol f));
    ("currencyNumericCode", fun f _ -> str (currency_numeric_code f));
    ( "bitcoinAddress",
      fun f a ->
        let o = opts a in
        let type_ =
          match string o "type" with
          | Some "legacy" -> Some `Legacy
          | Some "segwit" -> Some `Segwit
          | Some "bech32" -> Some `Bech32
          | Some "taproot" -> Some `Taproot
          | _ -> None
        in
        let network = match string o "network" with Some "testnet" -> Some `Testnet | _ -> None in
        str (bitcoin_address ?type_ ?network f) );
    ("litecoinAddress", fun f _ -> str (litecoin_address f));
    ( "creditCardNumber",
      fun f a -> let o = opts ~shorthand:"issuer" a in str (credit_card_number ?issuer:(string o "issuer") f) );
    ("creditCardCVV", fun f _ -> str (credit_card_cvv f));
    ("creditCardIssuer", fun f _ -> str (credit_card_issuer f));
    ("pin", fun f a -> let o = opts ~shorthand:"length" a in str (pin ?length:(int o "length") f));
    ("ethereumAddress", fun f _ -> str (ethereum_address f));
    ( "iban",
      fun f a ->
        let o = opts a in
        str (iban ?formatted:(bool o "formatted") ?country_code:(string o "countryCode") f) );
    ( "bic",
      fun f a -> let o = opts a in str (bic ?include_branch_code:(bool o "includeBranchCode") f) );
    ("transactionDescription", fun f _ -> str (transaction_description f));
  ]
