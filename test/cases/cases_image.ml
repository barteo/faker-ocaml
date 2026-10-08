open T
module I = Faker.Image

let fancy = "rgb(1, 2, 3) & #fff? \xc3\xa9\xe2\x82\xac\xf0\x9f\x98\x80"

let cases : case list =
  [
    ("avatar", fun f -> s (I.avatar f));
    ("avatarGitHub", fun f -> s (I.avatar_git_hub f));
    ("personPortrait", fun f -> s (I.person_portrait f));
    ("personPortrait/female", fun f -> s (I.person_portrait ~sex:`Female f));
    ("personPortrait/male", fun f -> s (I.person_portrait ~sex:`Male f));
    ("personPortrait/generic", fun f -> s (I.person_portrait ~sex:`Generic f));
    ("personPortrait/size", fun f -> s (I.person_portrait ~size:128 f));
    ("personPortrait/size32", fun f -> s (I.person_portrait ~size:32 f));
    ("personPortrait/both", fun f -> s (I.person_portrait ~sex:`Female ~size:64 f));
    ("personPortrait/genericSize", fun f -> s (I.person_portrait ~sex:`Generic ~size:256 f));
    ("url", fun f -> s (I.url f));
    ("url/width", fun f -> s (I.url ~width:128 f));
    ("url/height", fun f -> s (I.url ~height:64 f));
    ("url/both", fun f -> s (I.url ~width:300 ~height:200 f));
    ("urlLoremFlickr", fun f -> s (I.url_lorem_flickr f));
    ("urlLoremFlickr/size", fun f -> s (I.url_lorem_flickr ~width:640 ~height:480 f));
    ("urlLoremFlickr/height", fun f -> s (I.url_lorem_flickr ~height:480 f));
    ("urlLoremFlickr/category", fun f -> s (I.url_lorem_flickr ~category:"cats" f));
    ( "urlLoremFlickr/all",
      fun f -> s (I.url_lorem_flickr ~width:10 ~height:20 ~category:"nature" f) );
    ("urlPicsumPhotos", fun f -> s (I.url_picsum_photos f));
    ("urlPicsumPhotos/size", fun f -> s (I.url_picsum_photos ~width:128 ~height:256 f));
    ("urlPicsumPhotos/width", fun f -> s (I.url_picsum_photos ~width:128 f));
    ("urlPicsumPhotos/gray", fun f -> s (I.url_picsum_photos ~grayscale:true f));
    ("urlPicsumPhotos/nogray", fun f -> s (I.url_picsum_photos ~grayscale:false f));
    ("urlPicsumPhotos/blur0", fun f -> s (I.url_picsum_photos ~blur:0 f));
    ("urlPicsumPhotos/blur1", fun f -> s (I.url_picsum_photos ~blur:1 f));
    ("urlPicsumPhotos/blur10", fun f -> s (I.url_picsum_photos ~blur:10 f));
    ("urlPicsumPhotos/grayBlur", fun f -> s (I.url_picsum_photos ~grayscale:true ~blur:5 f));
    ("urlPicsumPhotos/noGrayBlur", fun f -> s (I.url_picsum_photos ~grayscale:false ~blur:3 f));
    ("urlPicsumPhotos/grayNoBlur", fun f -> s (I.url_picsum_photos ~grayscale:true ~blur:0 f));
    ( "urlPicsumPhotos/plain",
      fun f -> s (I.url_picsum_photos ~width:10 ~height:10 ~grayscale:false ~blur:0 f) );
    ("dataUri", fun f -> s (I.data_uri f));
    ("dataUri/svgUri", fun f -> s (I.data_uri ~type_:`Svg_uri f));
    ("dataUri/svgBase64", fun f -> s (I.data_uri ~type_:`Svg_base64 f));
    ("dataUri/size", fun f -> s (I.data_uri ~width:200 ~height:100 f));
    ("dataUri/odd", fun f -> s (I.data_uri ~width:101 ~height:33 ~type_:`Svg_uri f));
    ("dataUri/oddB64", fun f -> s (I.data_uri ~width:101 ~height:33 ~type_:`Svg_base64 f));
    ("dataUri/width", fun f -> s (I.data_uri ~width:7 f));
    ("dataUri/color", fun f -> s (I.data_uri ~color:"red" f));
    ("dataUri/colorCss", fun f -> s (I.data_uri ~color:fancy ~type_:`Svg_uri f));
    ("dataUri/colorCssB64", fun f -> s (I.data_uri ~color:fancy ~type_:`Svg_base64 f));
    ( "dataUri/specials",
      fun f ->
        s (I.data_uri ~width:1 ~height:2 ~color:"-_.!~*'()%/+=;:@$," ~type_:`Svg_uri f) );
    ("dataUri/all", fun f -> s (I.data_uri ~width:3 ~height:4 ~color:"blue" ~type_:`Svg_base64 f));
    ("dataUri/b64pad", fun f -> s (I.data_uri ~width:1 ~height:1 ~color:"a" ~type_:`Svg_base64 f));
    ( "dataUri/b64pad2",
      fun f -> s (I.data_uri ~width:1 ~height:1 ~color:"ab" ~type_:`Svg_base64 f) );
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{image.avatar}} {{image.avatarGitHub}} \
              {{image.personPortrait({\"sex\":\"male\",\"size\":64})}} \
              {{image.url({\"width\":5})}} {{image.urlLoremFlickr({\"category\":\"x\"})}} \
              {{image.urlPicsumPhotos({\"grayscale\":true,\"blur\":2})}} \
              {{image.dataUri({\"width\":2,\"height\":3,\"type\":\"svg-uri\"})}} \
              {{image.dataUri({\"type\":\"svg-base64\"})}}"
             f) );
    ("fake/noUrlPlaceholder", fun f -> s (Faker.Helpers.fake "{{image.urlPlaceholder}}" f));
    ("fake/noAvatarLegacy", fun f -> s (Faker.Helpers.fake "{{image.avatarLegacy}}" f));
  ]
