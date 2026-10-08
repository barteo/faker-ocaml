(* Data from src/modules/finance/iban.ts (generated from upstream). *)

type bban = { type_ : char; count : int }

type format = { country : string; bban : bban array }

let formats : format array =
  [|
    { country = "AL"; bban = [| { type_ = 'n'; count = 8 }; { type_ = 'c'; count = 16 } |] };
    { country = "AD"; bban = [| { type_ = 'n'; count = 8 }; { type_ = 'c'; count = 12 } |] };
    { country = "AT"; bban = [| { type_ = 'n'; count = 5 }; { type_ = 'n'; count = 11 } |] };
    { country = "AZ"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 20 } |] };
    { country = "BH"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'c'; count = 14 } |] };
    { country = "BE"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'n'; count = 9 } |] };
    { country = "BA"; bban = [| { type_ = 'n'; count = 6 }; { type_ = 'n'; count = 10 } |] };
    { country = "BR"; bban = [| { type_ = 'n'; count = 13 }; { type_ = 'n'; count = 10 }; { type_ = 'a'; count = 1 }; { type_ = 'c'; count = 1 } |] };
    { country = "BG"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 6 }; { type_ = 'c'; count = 8 } |] };
    { country = "CR"; bban = [| { type_ = 'n'; count = 1 }; { type_ = 'n'; count = 3 }; { type_ = 'n'; count = 14 } |] };
    { country = "HR"; bban = [| { type_ = 'n'; count = 7 }; { type_ = 'n'; count = 10 } |] };
    { country = "CY"; bban = [| { type_ = 'n'; count = 8 }; { type_ = 'c'; count = 16 } |] };
    { country = "CZ"; bban = [| { type_ = 'n'; count = 10 }; { type_ = 'n'; count = 10 } |] };
    { country = "DK"; bban = [| { type_ = 'n'; count = 4 }; { type_ = 'n'; count = 10 } |] };
    { country = "DO"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 20 } |] };
    { country = "TL"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'n'; count = 16 } |] };
    { country = "EE"; bban = [| { type_ = 'n'; count = 4 }; { type_ = 'n'; count = 12 } |] };
    { country = "FO"; bban = [| { type_ = 'n'; count = 4 }; { type_ = 'n'; count = 10 } |] };
    { country = "FI"; bban = [| { type_ = 'n'; count = 6 }; { type_ = 'n'; count = 8 } |] };
    { country = "FR"; bban = [| { type_ = 'n'; count = 10 }; { type_ = 'c'; count = 11 }; { type_ = 'n'; count = 2 } |] };
    { country = "GE"; bban = [| { type_ = 'a'; count = 2 }; { type_ = 'n'; count = 16 } |] };
    { country = "DE"; bban = [| { type_ = 'n'; count = 8 }; { type_ = 'n'; count = 10 } |] };
    { country = "GI"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'c'; count = 15 } |] };
    { country = "GR"; bban = [| { type_ = 'n'; count = 7 }; { type_ = 'c'; count = 16 } |] };
    { country = "GL"; bban = [| { type_ = 'n'; count = 4 }; { type_ = 'n'; count = 10 } |] };
    { country = "GT"; bban = [| { type_ = 'c'; count = 4 }; { type_ = 'c'; count = 4 }; { type_ = 'c'; count = 16 } |] };
    { country = "HU"; bban = [| { type_ = 'n'; count = 8 }; { type_ = 'n'; count = 16 } |] };
    { country = "IS"; bban = [| { type_ = 'n'; count = 6 }; { type_ = 'n'; count = 16 } |] };
    { country = "IE"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 6 }; { type_ = 'n'; count = 8 } |] };
    { country = "IL"; bban = [| { type_ = 'n'; count = 6 }; { type_ = 'n'; count = 13 } |] };
    { country = "IR"; bban = [| { type_ = 'n'; count = 22 } |] };
    { country = "IT"; bban = [| { type_ = 'a'; count = 1 }; { type_ = 'n'; count = 10 }; { type_ = 'c'; count = 12 } |] };
    { country = "JO"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 4 }; { type_ = 'n'; count = 18 } |] };
    { country = "KZ"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'c'; count = 13 } |] };
    { country = "XK"; bban = [| { type_ = 'n'; count = 4 }; { type_ = 'n'; count = 12 } |] };
    { country = "KW"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'c'; count = 22 } |] };
    { country = "LV"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'c'; count = 13 } |] };
    { country = "LB"; bban = [| { type_ = 'n'; count = 4 }; { type_ = 'c'; count = 20 } |] };
    { country = "LI"; bban = [| { type_ = 'n'; count = 5 }; { type_ = 'c'; count = 12 } |] };
    { country = "LT"; bban = [| { type_ = 'n'; count = 5 }; { type_ = 'n'; count = 11 } |] };
    { country = "LU"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'c'; count = 13 } |] };
    { country = "MK"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'c'; count = 10 }; { type_ = 'n'; count = 2 } |] };
    { country = "MT"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 5 }; { type_ = 'c'; count = 18 } |] };
    { country = "MR"; bban = [| { type_ = 'n'; count = 10 }; { type_ = 'n'; count = 13 } |] };
    { country = "MU"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 4 }; { type_ = 'n'; count = 15 }; { type_ = 'a'; count = 3 } |] };
    { country = "MC"; bban = [| { type_ = 'n'; count = 10 }; { type_ = 'c'; count = 11 }; { type_ = 'n'; count = 2 } |] };
    { country = "MD"; bban = [| { type_ = 'c'; count = 2 }; { type_ = 'c'; count = 18 } |] };
    { country = "ME"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'n'; count = 15 } |] };
    { country = "NL"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 10 } |] };
    { country = "NO"; bban = [| { type_ = 'n'; count = 4 }; { type_ = 'n'; count = 7 } |] };
    { country = "PK"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 16 } |] };
    { country = "PS"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 9 }; { type_ = 'n'; count = 12 } |] };
    { country = "PL"; bban = [| { type_ = 'n'; count = 8 }; { type_ = 'n'; count = 16 } |] };
    { country = "PT"; bban = [| { type_ = 'n'; count = 8 }; { type_ = 'n'; count = 13 } |] };
    { country = "QA"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'c'; count = 21 } |] };
    { country = "RO"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'c'; count = 16 } |] };
    { country = "SM"; bban = [| { type_ = 'a'; count = 1 }; { type_ = 'n'; count = 10 }; { type_ = 'c'; count = 12 } |] };
    { country = "SA"; bban = [| { type_ = 'n'; count = 2 }; { type_ = 'c'; count = 18 } |] };
    { country = "RS"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'n'; count = 15 } |] };
    { country = "SK"; bban = [| { type_ = 'n'; count = 10 }; { type_ = 'n'; count = 10 } |] };
    { country = "SI"; bban = [| { type_ = 'n'; count = 5 }; { type_ = 'n'; count = 10 } |] };
    { country = "ES"; bban = [| { type_ = 'n'; count = 10 }; { type_ = 'n'; count = 10 } |] };
    { country = "SE"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'n'; count = 17 } |] };
    { country = "CH"; bban = [| { type_ = 'n'; count = 5 }; { type_ = 'c'; count = 12 } |] };
    { country = "TN"; bban = [| { type_ = 'n'; count = 5 }; { type_ = 'n'; count = 15 } |] };
    { country = "TR"; bban = [| { type_ = 'n'; count = 5 }; { type_ = 'n'; count = 1 }; { type_ = 'n'; count = 16 } |] };
    { country = "AE"; bban = [| { type_ = 'n'; count = 3 }; { type_ = 'n'; count = 16 } |] };
    { country = "GB"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 6 }; { type_ = 'n'; count = 8 } |] };
    { country = "VG"; bban = [| { type_ = 'a'; count = 4 }; { type_ = 'n'; count = 16 } |] };
  |]

let iso3166 : string array =
  [| "AD"; "AE"; "AF"; "AG"; "AI"; "AL"; "AM"; "AO"; "AQ"; "AR"; "AS"; "AT"; "AU"; "AW"; "AX"; "AZ"; "BA"; "BB"; "BD"; "BE"; "BF"; "BG"; "BH"; "BI"; "BJ"; "BL"; "BM"; "BN"; "BO"; "BQ"; "BR"; "BS"; "BT"; "BV"; "BW"; "BY"; "BZ"; "CA"; "CC"; "CD"; "CF"; "CG"; "CH"; "CI"; "CK"; "CL"; "CM"; "CN"; "CO"; "CR"; "CU"; "CV"; "CW"; "CX"; "CY"; "CZ"; "DE"; "DJ"; "DK"; "DM"; "DO"; "DZ"; "EC"; "EE"; "EG"; "EH"; "ER"; "ES"; "ET"; "FI"; "FJ"; "FK"; "FM"; "FO"; "FR"; "GA"; "GB"; "GD"; "GE"; "GF"; "GG"; "GH"; "GI"; "GL"; "GM"; "GN"; "GP"; "GQ"; "GR"; "GS"; "GT"; "GU"; "GW"; "GY"; "HK"; "HM"; "HN"; "HR"; "HT"; "HU"; "ID"; "IE"; "IL"; "IM"; "IN"; "IO"; "IQ"; "IR"; "IS"; "IT"; "JE"; "JM"; "JO"; "JP"; "KE"; "KG"; "KH"; "KI"; "KM"; "KN"; "KP"; "KR"; "KW"; "KY"; "KZ"; "LA"; "LB"; "LC"; "LI"; "LK"; "LR"; "LS"; "LT"; "LU"; "LV"; "LY"; "MA"; "MC"; "MD"; "ME"; "MF"; "MG"; "MH"; "MK"; "ML"; "MM"; "MN"; "MO"; "MP"; "MQ"; "MR"; "MS"; "MT"; "MU"; "MV"; "MW"; "MX"; "MY"; "MZ"; "NA"; "NC"; "NE"; "NF"; "NG"; "NI"; "NL"; "NO"; "NP"; "NR"; "NU"; "NZ"; "OM"; "PA"; "PE"; "PF"; "PG"; "PH"; "PK"; "PL"; "PM"; "PN"; "PR"; "PS"; "PT"; "PW"; "PY"; "QA"; "RE"; "RO"; "RS"; "RU"; "RW"; "SA"; "SB"; "SC"; "SD"; "SE"; "SG"; "SH"; "SI"; "SJ"; "SK"; "SL"; "SM"; "SN"; "SO"; "SR"; "SS"; "ST"; "SV"; "SX"; "SY"; "SZ"; "TC"; "TD"; "TF"; "TG"; "TH"; "TJ"; "TK"; "TL"; "TM"; "TN"; "TO"; "TR"; "TT"; "TV"; "TW"; "TZ"; "UA"; "UG"; "UM"; "US"; "UY"; "UZ"; "VA"; "VC"; "VE"; "VG"; "VI"; "VN"; "VU"; "WF"; "WS"; "XK"; "YE"; "YT"; "ZA"; "ZM"; "ZW" |]

let alpha = Array.init 26 (fun i -> String.make 1 (Char.chr (65 + i)))
let pattern10 = [| "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09" |]
let pattern100 = [| "001"; "002"; "003"; "004"; "005"; "006"; "007"; "008"; "009" |]

let mod97 (digit_str : string) : int =
  let m = ref 0 in
  String.iter (fun c -> m := ((!m * 10) + (Char.code c - 48)) mod 97) digit_str;
  !m

(* str.replaceAll(/[A-Z]/gi, (match) => String(match.toUpperCase().codePointAt(0) - 55)) *)
let to_digit_string (str : string) : string =
  let b = Buffer.create (String.length str * 2) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' ->
          Buffer.add_string b (string_of_int (Char.code (Char.uppercase_ascii c) - 55))
      | c -> Buffer.add_char b c)
    str;
  Buffer.contents b
