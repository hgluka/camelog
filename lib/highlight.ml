open Core

let add_name name = function
  | `Assoc fields -> `Assoc (("name", `String name) :: fields)
  | json -> json

let builtin =
  Hilite.Grammars.[ocaml; ocaml_interface; dune; opam; add_name "shell" shell; diff]

let grammar_of_file path =
  try TmLanguage.of_yojson_exn (Yojson.Basic.from_file path :> TmLanguage.yojson) with
  | Yojson.Json_error msg | TmLanguage.Error msg -> failwithf "%s: %s" path msg ()

let grammars dir =
  let tm = TmLanguage.create () in
  List.iter builtin ~f:(fun json -> TmLanguage.add_grammar tm (TmLanguage.of_yojson_exn json));
  Option.iter dir ~f:(fun dir ->
    Sys_unix.ls_dir dir
    |> List.filter ~f:(String.is_suffix ~suffix:".json")
    |> List.sort ~compare:String.compare
    |> List.iter ~f:(fun file -> TmLanguage.add_grammar tm (grammar_of_file (Filename.concat dir file))));
  tm

let highlight tm doc =
  doc
  |> Hilite_markdown.transform ~lookup_method:`Name ~tm
  |> Hilite_markdown.transform ~lookup_method:`Filetype ~tm
