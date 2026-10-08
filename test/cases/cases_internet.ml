open T
module I = Faker.Internet

let names =
  [
    ("ascii", "Jane", "Doe");
    ("apos", "D'Angelo", "O'Brien");
    ("space", "Mary Ann", "Van der Berg");
    ("accent", "Jürgen", "Müller-Lüdenscheidt");
    ("cyrillic", "Пётр", "Иванов");
    ("greek", "Άλκηστις", "Παπαδοπούλου");
    ("cjk", "王", "小明");
    ("kana", "さくら", "ﾔﾏﾀﾞ");
    ("korean", "김", "민준");
    ("arabic", "آدم", "إبراهيم");
    ("hebrew", "דוד", "כֹּהֵן");
    ("armenian", "Արամ", "Պետրոսյան");
    ("emoji", "A😀", "𝐁𝐨𝐛");
    ("specialchars", "a!b#c$d", "..x..y..");
    ( "long",
      "Bartholomew-Maximilian-Alexander",
      "Wolfeschlegelsteinhausenbergerdorff" );
    ("ligature", "ﬁona", "Ⅻ①");
  ]

let networks : (string * I.ipv4_network) list =
  [
    ("any", `Any);
    ("loopback", `Loopback);
    ("private-a", `Private_a);
    ("private-b", `Private_b);
    ("private-c", `Private_c);
    ("test-net-1", `Test_net_1);
    ("test-net-2", `Test_net_2);
    ("test-net-3", `Test_net_3);
    ("link-local", `Link_local);
    ("multicast", `Multicast);
  ]

let cidrs =
  [
    "192.168.1.0/24";
    "10.0.0.1/32";
    "010.0.0.1/32";
    "0.0.0.0/0";
    "255.255.255.255/31";
    "255.255.255.255/1";
    "128.0.0.0/1";
    "172.31.255.7/30";
    "1.2.3.4/9";
    "abc";
    "1.2.3.4/33";
    "1.2.3.4/99";
    "256.0.0.0/8";
    "1.2.3/8";
    "1.2.3.4";
    "1.2.3.4/";
    "1.2.3.4/123";
    "1234.0.0.0/8";
    " 1.2.3.4/8";
    "1.2.3.4/8 ";
  ]

let fake p f = s (Faker.Helpers.fake p f)
let is_upper c = c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_digit c || is_upper c || (c >= 'a' && c <= 'z')

let cases : case list =
  [
    ("email", fun f -> s (I.email f));
    ("email/first", fun f -> s (I.email ~first_name:"Jane" f));
    ("email/last", fun f -> s (I.email ~last_name:"Doe" f));
    ("email/provider", fun f -> s (I.email ~provider:"example.fakerjs.dev" f));
    ("email/special", fun f -> s (I.email ~allow_special_characters:true f));
    ( "email/special-names",
      fun f ->
        s
          (I.email ~first_name:"Jane" ~last_name:"Doe"
             ~allow_special_characters:true f) );
    ("email/dots", fun f -> s (I.email ~first_name:".Jane." ~last_name:"." f));
    ("email/emptylast", fun f -> s (I.email ~first_name:"Jane" ~last_name:"" f));
    ("exampleEmail", fun f -> s (I.example_email f));
    ( "exampleEmail/names",
      fun f -> s (I.example_email ~first_name:"Jane" ~last_name:"Doe" f) );
    ( "exampleEmail/special",
      fun f -> s (I.example_email ~allow_special_characters:true f) );
    ("username", fun f -> s (I.username f));
    ("username/first", fun f -> s (I.username ~first_name:"Jane" f));
    ("username/last", fun f -> s (I.username ~last_name:"Doe" f));
    ( "username/emptylast",
      fun f -> s (I.username ~first_name:"Jane" ~last_name:"" f) );
    ("displayName", fun f -> s (I.display_name f));
    ("displayName/first", fun f -> s (I.display_name ~first_name:"Jane" f));
    ("displayName/last", fun f -> s (I.display_name ~last_name:"Doe" f));
    ("protocol", fun f -> s (I.http_protocol_to_string (I.protocol f)));
    ("httpMethod", fun f -> s (I.http_method_to_string (I.http_method f)));
    ("httpStatusCode", fun f -> i (I.http_status_code f));
    ( "httpStatusCode/success",
      fun f -> i (I.http_status_code ~types:[ `Success ] f) );
    ( "httpStatusCode/errors",
      fun f -> i (I.http_status_code ~types:[ `Client_error; `Server_error ] f)
    );
    ( "httpStatusCode/all",
      fun f ->
        i
          (I.http_status_code
             ~types:
               [
                 `Informational;
                 `Success;
                 `Client_error;
                 `Server_error;
                 `Redirection;
               ]
             f) );
    ("httpStatusCode/empty", fun f -> i (I.http_status_code ~types:[] f));
    ("url", fun f -> s (I.url f));
    ("url/slash", fun f -> s (I.url ~append_slash:true f));
    ("url/noslash", fun f -> s (I.url ~append_slash:false f));
    ("url/http", fun f -> s (I.url ~protocol:`Http f));
    ("url/https", fun f -> s (I.url ~protocol:`Https ~append_slash:true f));
    ("domainName", fun f -> s (I.domain_name f));
    ("domainSuffix", fun f -> s (I.domain_suffix f));
    ("domainWord", fun f -> s (I.domain_word f));
    ("ip", fun f -> s (I.ip f));
    ("ipv4", fun f -> s (I.ipv4 f));
  ]
  @ List.map
      (fun (n, v) -> ("ipv4/" ^ n, fun f -> s (I.ipv4 ~network:v f)))
      networks
  @ List.map
      (fun c -> ("ipv4/cidr:" ^ c, fun f -> s (I.ipv4 ~cidr_block:c f)))
      cidrs
  @ [
      ( "ipv4/both",
        fun f -> s (I.ipv4 ~cidr_block:"192.0.2.0/30" ~network:`Loopback f) );
      ("ipv6", fun f -> s (I.ipv6 f));
      ("port", fun f -> i (I.port f));
      ("userAgent", fun f -> s (I.user_agent f));
      ("mac", fun f -> s (I.mac f));
      ("mac/dash", fun f -> s (I.mac ~separator:"-" f));
      ("mac/empty", fun f -> s (I.mac ~separator:"" f));
      ("mac/invalid", fun f -> s (I.mac ~separator:"|" f));
      ("mac/obj", fun f -> s (I.mac ~separator:"-" f));
      ("mac/objdefault", fun f -> s (I.mac f));
      ("password", fun f -> s (I.password f));
      ("password/5", fun f -> s (I.password ~length:5 f));
      ("password/0", fun f -> s (I.password ~length:0 f));
      ("password/memorable", fun f -> s (I.password ~memorable:true f));
      ( "password/memorable20",
        fun f -> s (I.password ~length:20 ~memorable:true f) );
      ("password/upper", fun f -> s (I.password ~pattern:is_upper f));
      ("password/digit", fun f -> s (I.password ~length:8 ~pattern:is_digit f));
      ( "password/symbols",
        fun f ->
          s (I.password ~length:12 ~pattern:(fun c -> not (is_alnum c)) f) );
      ("password/prefix", fun f -> s (I.password ~prefix:"pre-" f));
      ( "password/longprefix",
        fun f -> s (I.password ~length:5 ~prefix:"abcdefghij" f) );
      ( "password/memorableprefix",
        fun f -> s (I.password ~length:10 ~memorable:true ~prefix:"xyz" f) );
      ( "password/memorablevowel",
        fun f -> s (I.password ~length:10 ~memorable:true ~prefix:"Ba" f) );
      ( "password/memorablepattern",
        fun f -> s (I.password ~length:10 ~memorable:true ~pattern:is_digit f)
      );
      ( "password/unicodeprefix",
        fun f -> s (I.password ~length:10 ~prefix:"é😀" f) );
      ("emoji", fun f -> s (I.emoji f));
      ("emoji/flag", fun f -> s (I.emoji ~types:[ `Flag ] f));
      ("emoji/foodnature", fun f -> s (I.emoji ~types:[ `Food; `Nature ] f));
      ("emoji/empty", fun f -> s (I.emoji ~types:[] f));
      ("jwtAlgorithm", fun f -> s (I.jwt_algorithm f));
      ("jwt", fun f -> s (I.jwt f));
      ( "jwt/ref",
        fun f ->
          s
            (I.jwt
               ~ref_date:(Faker.Date_util.of_iso "2020-01-01T00:00:00.000Z")
               f) );
      ("jwt/refnum", fun f -> s (I.jwt ~ref_date:1600000000123.0 f));
      ("jwt/referr", fun f -> s (I.jwt ~ref_date:nan f));
      ( "jwt/header",
        fun f ->
          s
            (I.jwt
               ~header:
                 (J.Obj
                    [
                      ("alg", J.Str "none");
                      ("kid", J.Str "ключ");
                      ("n", J.Num 1.5);
                      ("b", J.Bool true);
                      ("z", J.Null);
                      ("a", J.Arr [| J.Num 1.0; J.Str "x" |]);
                    ])
               f) );
      ( "jwt/payload",
        fun f ->
          s
            (I.jwt
               ~payload:
                 (J.Obj
                    [
                      ("sub", J.Str "abc");
                      ("admin", J.Bool true);
                      ("nested", J.Obj [ ("x", J.Str "é\n\"") ]);
                    ])
               f) );
      ("jwt/both", fun f -> s (I.jwt ~header:(J.Obj []) ~payload:(J.Obj []) f));
      ( "fake/misc",
        fake
          "{{internet.email}}|{{internet.exampleEmail}}|{{internet.username}}|{{internet.displayName}}|{{internet.protocol}}|{{internet.httpMethod}}|{{internet.httpStatusCode}}|{{internet.url}}|{{internet.domainName}}|{{internet.domainSuffix}}|{{internet.domainWord}}|{{internet.ip}}|{{internet.ipv4}}|{{internet.ipv6}}|{{internet.port}}|{{internet.mac}}|{{internet.password}}|{{internet.emoji}}|{{internet.jwtAlgorithm}}"
      );
      ("fake/userAgent", fake "{{internet.userAgent}}");
      ("fake/jwt", fake "{{internet.jwt}}");
      ( "fake/args",
        fake
          "{{internet.email({\"firstName\":\"Jane\",\"provider\":\"x.dev\",\"allowSpecialCharacters\":true})}}|{{internet.username({\"lastName\":\"Doe\"})}}|{{internet.displayName({\"firstName\":\"Jo\"})}}|{{internet.httpStatusCode({\"types\":[\"success\"]})}}|{{internet.url({\"appendSlash\":true,\"protocol\":\"http\"})}}|{{internet.ipv4({\"network\":\"private-c\"})}}|{{internet.ipv4({\"cidrBlock\":\"10.1.0.0/16\"})}}|{{internet.mac(\"-\")}}|{{internet.mac({\"separator\":\"\"})}}|{{internet.password({\"length\":6,\"memorable\":true})}}|{{internet.emoji({\"types\":[\"flag\"]})}}"
      );
      ( "fake/jwtargs",
        fake
          "{{internet.jwt({\"header\":{\"alg\":\"x\"},\"refDate\":\"2020-01-01T00:00:00.000Z\"})}}"
      );
      ("fake/ipv4bad", fake "{{internet.ipv4({\"network\":\"bogus\"})}}");
    ]
  @ List.concat_map
      (fun (id, first, last) ->
        [
          ( "username/" ^ id,
            fun f -> s (I.username ~first_name:first ~last_name:last f) );
          ( "username/" ^ id ^ "/first",
            fun f -> s (I.username ~first_name:(first ^ last) f) );
          ( "email/" ^ id,
            fun f -> s (I.email ~first_name:first ~last_name:last f) );
          ( "displayName/" ^ id,
            fun f -> s (I.display_name ~first_name:first ~last_name:last f) );
        ])
      names
