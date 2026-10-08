open T
module Sy = Faker.System

let fake p f = s (Faker.Helpers.fake p f)

let cases : case list =
  [
    ("fileName", fun f -> s (Sy.file_name f));
    ("fileName/0", fun f -> s (Sy.file_name ~extension_count:(`N 0) f));
    ("fileName/3", fun f -> s (Sy.file_name ~extension_count:(`N 3) f));
    ( "fileName/range",
      fun f -> s (Sy.file_name ~extension_count:(`Range (1, 4)) f) );
    ( "fileName/range0",
      fun f -> s (Sy.file_name ~extension_count:(`Range (0, 2)) f) );
    ("commonFileName", fun f -> s (Sy.common_file_name f));
    ("commonFileName/ext", fun f -> s (Sy.common_file_name ~extension:"txt" f));
    ("commonFileName/empty", fun f -> s (Sy.common_file_name ~extension:"" f));
    ("mimeType", fun f -> s (Sy.mime_type f));
    ("commonFileType", fun f -> s (Sy.common_file_type f));
    ("commonFileExt", fun f -> s (Sy.common_file_ext f));
    ("fileType", fun f -> s (Sy.file_type f));
    ("fileExt", fun f -> s (Sy.file_ext f));
    ("fileExt/png", fun f -> s (Sy.file_ext ~mime_type:"image/png" f));
    ("fileExt/jpeg", fun f -> s (Sy.file_ext ~mime_type:"image/jpeg" f));
    ("fileExt/mpeg", fun f -> s (Sy.file_ext ~mime_type:"audio/mpeg" f));
    ("fileExt/err", fun f -> s (Sy.file_ext ~mime_type:"foo/bar" f));
    ("fileExt/errempty", fun f -> s (Sy.file_ext ~mime_type:"" f));
    ("directoryPath", fun f -> s (Sy.directory_path f));
    ("filePath", fun f -> s (Sy.file_path f));
    ("semver", fun f -> s (Sy.semver f));
    ("networkInterface", fun f -> s (Sy.network_interface f));
  ]
  @ List.map
      (fun (n, t) ->
        ( "networkInterface/type:" ^ n,
          fun f -> s (Sy.network_interface ~interface_type:t f) ))
      [ ("en", `En); ("wl", `Wl); ("ww", `Ww) ]
  @ List.map
      (fun (n, sc) ->
        ( "networkInterface/schema:" ^ n,
          fun f -> s (Sy.network_interface ~interface_schema:sc f) ))
      [ ("index", `Index); ("slot", `Slot); ("mac", `Mac); ("pci", `Pci) ]
  @ [
      ( "networkInterface/both",
        fun f ->
          s (Sy.network_interface ~interface_type:`Wl ~interface_schema:`Pci f)
      );
      ("cron", fun f -> s (Sy.cron f));
      ("cron/year", fun f -> s (Sy.cron ~include_year:true f));
      ("cron/nonstandard", fun f -> s (Sy.cron ~include_non_standard:true f));
      ( "cron/both",
        fun f -> s (Sy.cron ~include_year:true ~include_non_standard:true f) );
      ( "cron/false",
        fun f -> s (Sy.cron ~include_year:false ~include_non_standard:false f)
      );
      ( "fake/misc",
        fake
          "{{system.fileName}}|{{system.commonFileName}}|{{system.mimeType}}|{{system.commonFileType}}|{{system.commonFileExt}}|{{system.fileType}}|{{system.fileExt}}|{{system.directoryPath}}|{{system.filePath}}|{{system.semver}}|{{system.networkInterface}}|{{system.cron}}"
      );
      ( "fake/args",
        fake
          "{{system.fileName({\"extensionCount\":2})}}|{{system.commonFileName(\"pdf\")}}|{{system.fileExt(\"image/gif\")}}|{{system.networkInterface({\"interfaceType\":\"ww\",\"interfaceSchema\":\"slot\"})}}|{{system.cron({\"includeYear\":true,\"includeNonStandard\":true})}}"
      );
      ("fake/fileExtErr", fake "{{system.fileExt(\"nope/nope\")}}");
    ]
