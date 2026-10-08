open T
module C = Faker.Color

let fs a = arr n a

let cases : case list =
  [
    ("human", fun f -> s (C.human f));
    ("space", fun f -> s (C.space f));
    ("cssSupportedFunction", fun f -> s (C.css_supported_function f));
    ("cssSupportedSpace", fun f -> s (C.css_supported_space f));
    ("rgb", fun f -> s (C.rgb f));
    ("rgb/upper", fun f -> s (C.rgb ~casing:`Upper f));
    ("rgb/mixed", fun f -> s (C.rgb ~casing:`Mixed ~prefix:"0x" f));
    ("rgb/noprefix", fun f -> s (C.rgb ~prefix:"" ~include_alpha:true f));
    ( "rgb/hexalpha",
      fun f -> s (C.rgb ~format:`Hex ~include_alpha:true ~casing:`Upper f) );
    ("rgb/css", fun f -> s (C.rgb ~format:`Css f));
    ("rgb/cssalpha", fun f -> s (C.rgb ~format:`Css ~include_alpha:true f));
    ("rgb/binary", fun f -> s (C.rgb ~format:`Binary f));
    ("rgb/binaryalpha", fun f -> s (C.rgb ~format:`Binary ~include_alpha:true f));
    ("rgb/decimal", fun f -> fs (C.rgb_decimal f));
    ("rgb/decimalalpha", fun f -> fs (C.rgb_decimal ~include_alpha:true f));
    ("cmyk", fun f -> fs (C.cmyk f));
    ("cmyk/css", fun f -> s (C.cmyk_string f));
    ("cmyk/binary", fun f -> s (C.cmyk_string ~format:`Binary f));
    ("hsl", fun f -> fs (C.hsl f));
    ("hsl/alpha", fun f -> fs (C.hsl ~include_alpha:true f));
    ("hsl/css", fun f -> s (C.hsl_string ~format:`Css f));
    ("hsl/cssalpha", fun f -> s (C.hsl_string ~include_alpha:true f));
    ("hsl/binary", fun f -> s (C.hsl_string ~format:`Binary f));
    ( "hsl/binaryalpha",
      fun f -> s (C.hsl_string ~format:`Binary ~include_alpha:true f) );
    ("hwb", fun f -> fs (C.hwb f));
    ("hwb/css", fun f -> s (C.hwb_string f));
    ("hwb/binary", fun f -> s (C.hwb_string ~format:`Binary f));
    ("lab", fun f -> fs (C.lab f));
    ("lab/css", fun f -> s (C.lab_string f));
    ("lab/binary", fun f -> s (C.lab_string ~format:`Binary f));
    ("lch", fun f -> fs (C.lch f));
    ("lch/css", fun f -> s (C.lch_string f));
    ("lch/binary", fun f -> s (C.lch_string ~format:`Binary f));
    ("colorByCSSColorSpace", fun f -> fs (C.color_by_css_color_space f));
    ( "colorByCSSColorSpace/css",
      fun f -> s (C.color_by_css_color_space_string f) );
    ( "colorByCSSColorSpace/p3",
      fun f -> s (C.color_by_css_color_space_string ~space:`Display_p3 f) );
    ( "colorByCSSColorSpace/rec",
      fun f -> s (C.color_by_css_color_space_string ~space:`Rec2020 f) );
    ( "colorByCSSColorSpace/a98",
      fun f -> s (C.color_by_css_color_space_string ~space:`A98_rgb f) );
    ( "colorByCSSColorSpace/pro",
      fun f -> s (C.color_by_css_color_space_string ~space:`Prophoto_rgb f) );
    ( "colorByCSSColorSpace/binary",
      fun f ->
        s
          (C.color_by_css_color_space_string ~format:`Binary ~space:`Display_p3
             f) );
    ( "fake",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{color.human}} {{color.rgb({\"format\":\"css\"})}} \
              {{color.hsl}} {{color.lab({\"format\":\"binary\"})}}"
             f) );
  ]
