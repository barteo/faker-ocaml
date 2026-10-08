open T
module G = Faker.Git

let iso = Faker.Date_util.of_iso
let r = iso "2020-02-29T23:59:59.999Z"
let old = iso "1960-06-15T08:09:10.111Z"
let fake p f = s (Faker.Helpers.fake p f)

let cases : case list =
  [
    ("branch", fun f -> s (G.branch f));
    ("commitEntry", fun f -> s (G.commit_entry f));
    ("commitEntry/merge", fun f -> s (G.commit_entry ~merge:true f));
    ("commitEntry/nomerge", fun f -> s (G.commit_entry ~merge:false f));
    ("commitEntry/lf", fun f -> s (G.commit_entry ~eol:`LF f));
    ("commitEntry/crlf", fun f -> s (G.commit_entry ~eol:`CRLF ~merge:true f));
    ("commitEntry/ref", fun f -> s (G.commit_entry ~ref_date:r f));
    ( "commitEntry/refnum",
      fun f -> s (G.commit_entry ~ref_date:1600000000123.0 f) );
    ( "commitEntry/old",
      fun f -> s (G.commit_entry ~ref_date:old ~eol:`LF ~merge:true f) );
    ("commitEntry/referr", fun f -> s (G.commit_entry ~ref_date:nan f));
    ("commitMessage", fun f -> s (G.commit_message f));
    ("commitDate", fun f -> s (G.commit_date f));
    ("commitDate/ref", fun f -> s (G.commit_date ~ref_date:r f));
    ("commitDate/refnum", fun f -> s (G.commit_date ~ref_date:1600000000123.0 f));
    ("commitDate/old", fun f -> s (G.commit_date ~ref_date:old f));
    ("commitDate/epoch", fun f -> s (G.commit_date ~ref_date:86400000.0 f));
    ("commitDate/referr", fun f -> s (G.commit_date ~ref_date:nan f));
    ("commitSha", fun f -> s (G.commit_sha f));
    ("commitSha/7", fun f -> s (G.commit_sha ~length:7 f));
    ("commitSha/1", fun f -> s (G.commit_sha ~length:1 f));
    ("commitSha/0", fun f -> s (G.commit_sha ~length:0 f));
    ("commitSha/64", fun f -> s (G.commit_sha ~length:64 f));
    ( "fake/misc",
      fake
        "{{git.branch}}|{{git.commitMessage}}|{{git.commitDate}}|{{git.commitSha}}|{{git.commitSha({\"length\":7})}}|{{git.commitDate({\"refDate\":\"2020-01-01T00:00:00.000Z\"})}}"
    );
    ( "fake/entry",
      fake
        "{{git.commitEntry({\"merge\":true,\"eol\":\"LF\",\"refDate\":\"2020-01-01T00:00:00.000Z\"})}}"
    );
    ("fake/entrydefault", fake "{{git.commitEntry}}");
  ]
