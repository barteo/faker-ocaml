open T
module D = Faker.Date

let iso = Faker.Date_util.of_iso
let r = iso "2020-01-01T00:00:00.000Z"
let r2030 = iso "2030-01-01T00:00:00.000Z"
let leap = iso "2024-02-29T12:34:56.789Z"
let old = iso "1960-06-15T08:09:10.111Z"

let cases : case list =
  [
    ("anytime", fun f -> date (D.anytime f));
    ("anytime/ref", fun f -> date (D.anytime ~ref_date:r f));
    ("anytime/refnum", fun f -> date (D.anytime ~ref_date:1600000000123.7 f));
    ("anytime/err", fun f -> date (D.anytime ~ref_date:nan f));
    ("past", fun f -> date (D.past f));
    ("past/10", fun f -> date (D.past ~years:(`N 10) f));
    ("past/range", fun f -> date (D.past ~years:(`Range (4, 7)) f));
    ("past/ref", fun f -> date (D.past ~years:(`N 10) ~ref_date:r f));
    ("past/leap", fun f -> date (D.past ~ref_date:leap f));
    ("past/old", fun f -> date (D.past ~years:(`N 5) ~ref_date:old f));
    ("past/err0", fun f -> date (D.past ~years:(`N 0) f));
    ("past/errneg", fun f -> date (D.past ~years:(`N (-3)) f));
    ("past/errrange", fun f -> date (D.past ~years:(`Range (5, 5)) f));
    ("past/errref", fun f -> date (D.past ~ref_date:nan f));
    ("past/errhuge", fun f -> date (D.past ~years:(`N 300000) f));
    ("future", fun f -> date (D.future f));
    ("future/10", fun f -> date (D.future ~years:(`N 10) f));
    ("future/range", fun f -> date (D.future ~years:(`Range (4, 7)) f));
    ("future/ref", fun f -> date (D.future ~years:(`N 10) ~ref_date:r f));
    ("future/leap", fun f -> date (D.future ~years:(`N 3) ~ref_date:leap f));
    ("future/err0", fun f -> date (D.future ~years:(`N 0) f));
    ("future/errrange", fun f -> date (D.future ~years:(`Range (6, 2)) f));
    ("future/errhuge", fun f -> date (D.future ~years:(`N 300000) f));
    ("between", fun f -> date (D.between ~from:r ~to_:r2030 f));
    ("between/num", fun f -> date (D.between ~from:0.0 ~to_:1000000.0 f));
    ( "between/old",
      fun f ->
        date (D.between ~from:(iso "1800-01-01T00:00:00.000Z") ~to_:old f) );
    ("between/same", fun f -> date (D.between ~from:r ~to_:r f));
    ("between/err", fun f -> date (D.between ~from:r2030 ~to_:r f));
    ("between/errfrom", fun f -> date (D.between ~from:nan ~to_:r f));
    ("between/errto", fun f -> date (D.between ~from:r ~to_:9e15 f));
    ("betweens", fun f -> arr date (D.betweens ~from:r ~to_:r2030 f));
    ( "betweens/2",
      fun f -> arr date (D.betweens ~count:(`N 2) ~from:r ~to_:r2030 f) );
    ( "betweens/range",
      fun f ->
        arr date (D.betweens ~count:(`Range (2, 6)) ~from:0.0 ~to_:100000.0 f)
    );
    ( "betweens/0",
      fun f -> arr date (D.betweens ~count:(`N 0) ~from:10.0 ~to_:0.0 f) );
    ("betweens/err", fun f -> arr date (D.betweens ~from:10.0 ~to_:0.0 f));
    ("recent", fun f -> date (D.recent f));
    ("recent/10", fun f -> date (D.recent ~days:(`N 10) f));
    ("recent/range", fun f -> date (D.recent ~days:(`Range (4, 7)) f));
    ( "recent/ref",
      fun f ->
        date
          (D.recent ~days:(`N 40) ~ref_date:(iso "2025-03-01T10:00:00.000Z") f)
    );
    ("recent/err0", fun f -> date (D.recent ~days:(`N 0) f));
    ("recent/errrange", fun f -> date (D.recent ~days:(`Range (3, 1)) f));
    ("soon", fun f -> date (D.soon f));
    ("soon/10", fun f -> date (D.soon ~days:(`N 10) f));
    ("soon/range", fun f -> date (D.soon ~days:(`Range (4, 7)) f));
    ("soon/ref", fun f -> date (D.soon ~days:(`N 400) ~ref_date:leap f));
    ("soon/err0", fun f -> date (D.soon ~days:(`N (-1)) f));
    ("soon/errrange", fun f -> date (D.soon ~days:(`Range (2, 2)) f));
    ("birthdate", fun f -> date (D.birthdate f));
    ("birthdate/age", fun f -> date (D.birthdate ~mode:`Age ~min:18 ~max:65 f));
    ( "birthdate/ageref",
      fun f -> date (D.birthdate ~mode:`Age ~min:3 ~max:3 ~ref_date:leap f) );
    ("birthdate/agedefault", fun f -> date (D.birthdate ~ref_date:old f));
    ( "birthdate/ageerr",
      fun f -> date (D.birthdate ~mode:`Age ~min:40 ~max:20 f) );
    ( "birthdate/year",
      fun f -> date (D.birthdate ~mode:`Year ~min:1900 ~max:2000 f) );
    ( "birthdate/yearsame",
      fun f -> date (D.birthdate ~mode:`Year ~min:1999 ~max:1999 f) );
    ("birthdate/yeardefault", fun f -> date (D.birthdate ~mode:`Year f));
    ( "birthdate/yearerr",
      fun f -> date (D.birthdate ~mode:`Year ~min:2000 ~max:1990 f) );
    ( "birthdate/yearhuge",
      fun f -> date (D.birthdate ~mode:`Year ~min:300000 ~max:300001 f) );
    ( "birthdate/errref",
      fun f -> date (D.birthdate ~mode:`Year ~min:1 ~max:2 ~ref_date:nan f) );
    ("month", fun f -> s (D.month f));
    ("month/abbr", fun f -> s (D.month ~abbreviated:true f));
    ("month/context", fun f -> s (D.month ~context:true f));
    ("month/abbrctx", fun f -> s (D.month ~abbreviated:true ~context:true f));
    ("weekday", fun f -> s (D.weekday f));
    ("weekday/abbr", fun f -> s (D.weekday ~abbreviated:true f));
    ("weekday/context", fun f -> s (D.weekday ~context:true f));
    ("weekday/abbrctx", fun f -> s (D.weekday ~abbreviated:true ~context:true f));
    ("timeZone", fun f -> s (D.time_zone f));
    ( "fake/month",
      fun f ->
        s
          (Faker.Helpers.fake
             "{{date.month}} / {{date.weekday({\"abbreviated\":true})}} / \
              {{date.timeZone}}"
             f) );
    ( "fake/errref",
      fun f -> s (Faker.Helpers.fake "{{date.past({\"refDate\":\"foo\"})}}" f)
    );
    ( "fake/errbetween",
      fun f ->
        s (Faker.Helpers.fake "{{date.between({\"from\":\"2020-01-01\"})}}" f)
    );
  ]
