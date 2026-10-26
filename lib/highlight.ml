open Core

let add_name name = function
  | `Assoc fields -> `Assoc (("name", `String name) :: fields)
  | json -> json

let builtin =
  Hilite.Grammars.[ocaml; ocaml_interface; dune; opam; add_name "shell" shell; diff]

let rec normalize : TmLanguage.yojson -> TmLanguage.yojson = function
  | `Assoc fields ->
    `Assoc (List.map fields ~f:(fun (key, value) ->
      match key, value with
      | "repository", `Assoc entries ->
        key, `Assoc (List.map entries ~f:(fun (name, entry) ->
          match entry with
          | `List _ -> name, `Assoc ["patterns", normalize entry]
          | _ -> name, normalize entry))
      | ("captures" | "beginCaptures" | "endCaptures" | "whileCaptures"), `Assoc entries ->
        key, `Assoc (List.filter_map entries ~f:(fun (capture, entry) ->
          match entry with
          | `Assoc _ -> Some (capture, normalize entry)
          | _ -> None))
      | _ -> key, normalize value))
  | `List items -> `List (List.map items ~f:normalize)
  | json -> json

let grammar_of_file path =
  try TmLanguage.of_yojson_exn (normalize (Yojson.Basic.from_file path :> TmLanguage.yojson)) with
  | Yojson.Json_error msg | TmLanguage.Error msg -> failwithf "%s: %s" path msg ()

let grammars dir =
  let tm = TmLanguage.create () in
  List.iter builtin ~f:(fun json -> TmLanguage.add_grammar tm (TmLanguage.of_yojson_exn json));
  (match Sys_unix.is_directory dir with
   | `Yes ->
     Sys_unix.ls_dir dir
     |> List.filter ~f:(String.is_suffix ~suffix:".json")
     |> List.sort ~compare:String.compare
     |> List.iter ~f:(fun file -> TmLanguage.add_grammar tm (grammar_of_file (Filename.concat dir file)))
   | `No | `Unknown -> ());
  tm

let highlight tm doc =
  doc
  |> Hilite_markdown.transform ~lookup_method:`Name ~tm
  |> Hilite_markdown.transform ~lookup_method:`Filetype ~tm
