open T
module U = Faker.Unicode

let samples =
  [|
    "ΑΣ ΣΑ ΌΣΟΣ. Σ ΑΣ\u{301} ΑΣ'Α";
    "İstanbul KELVIN\u{212A} straße ǅemal ﬁnale ŉ";
    "ǈǋǲ ΐ ᾳ ﬃ 𐐨𐑐 𞤢 ⓐ Ⅻ";
    "հայերէն եւ և Ⴀ ა ꙁ";
  |]

let stacked =
  [|
    "\u{5e9}\u{5c1}\u{5b8}\u{5dc}\u{5d5}\u{5b9}\u{5dd}";
    "\u{628}\u{651}\u{64e}\u{628}\u{64e}\u{651}";
    "a\u{301}\u{316}\u{334}b\u{327}\u{301}";
    "Å\u{323} ẫ ǖ ᾷ 가각 ㎏ 𝐇𝐞𝐥𝐥𝐨 ①";
  |]

let numbers =
  [| ""; " 12 "; "1e3"; "0x1F"; "0o17"; "0b101"; ".5"; "5."; "+1"; "-Infinity"; "NaN"; "1_000"; "0x"; "١"; "12px"; " \t\n" |]

let cases : case list =
  [
    ("toUpperCase", fun _ -> ss (Array.map U.js_upper samples));
    ("toLowerCase", fun _ -> ss (Array.map U.js_lower samples));
    ( "upperFirst",
      fun _ -> ss (Array.map U.js_upper_first (Array.append samples [| "ǆa"; "ßa"; "𐐨x"; "ŉx" |])) );
    ("nfkd", fun _ -> ss (Array.map U.nfkd (Array.append samples stacked)));
    ("slugify", fun _ -> ss (Array.map Faker.Helpers.slugify (Array.append samples stacked)));
    ( "username",
      fun f -> ss (Array.map (fun s -> Faker.Internet.username ~first_name:s ~last_name:"x" f) stacked) );
    ("number", fun _ -> ss (Array.map (fun s -> Faker.Js.number_to_string (Faker.Js.to_number s)) numbers));
  ]
