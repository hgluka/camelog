open Core

type formula = { tex: string; display: bool }

let marker i = sprintf "<!--camelog-math-%d-->" i

let extract doc =
  let formulas = ref [] in
  let add formula =
    formulas := formula :: !formulas;
    marker (List.length !formulas - 1) in
  let inline _ = function
    | Cmarkit.Inline.Ext_math_span (span, meta) ->
      let m = Cmarkit.Inline.Math_span.(add { tex = tex span; display = display span }) in
      Cmarkit.Mapper.ret (Cmarkit.Inline.Raw_html (Cmarkit.Block_line.tight_list_of_string m, meta))
    | _ -> Cmarkit.Mapper.default in
  let block _ = function
    | Cmarkit.Block.Ext_math_block (code, meta) ->
      let tex = Cmarkit.Block.Code_block.code code |> List.map ~f:Cmarkit.Block_line.to_string |> String.concat ~sep:"\n" in
      let m = add { tex; display = true } in
      Cmarkit.Mapper.ret (Cmarkit.Block.Html_block (Cmarkit.Block_line.list_of_string m, meta))
    | _ -> Cmarkit.Mapper.default in
  let doc = Cmarkit.Mapper.map_doc (Cmarkit.Mapper.make ~inline ~block ()) doc in
  doc, List.rev !formulas

let pandoc_input formulas =
  List.map formulas ~f:(fun { tex; display } ->
    let delim = if display then "$$" else "$" in
    delim ^ String.strip tex ^ delim)
  |> String.concat ~sep:"\n\n"

let run_pandoc input =
  let args = [|"pandoc"; "--from=markdown"; "--to=html"; "--mathml"; "--wrap=none"; "--fail-if-warnings"|] in
  let stdout, stdin, stderr =
    try Caml_unix.open_process_args_full "pandoc" args (Caml_unix.environment ()) with
    | Caml_unix.Unix_error (Caml_unix.ENOENT, _, _) -> failwith "math needs pandoc, which was not found on PATH" in
  Out_channel.output_string stdin input;
  Out_channel.close stdin;
  let output = In_channel.input_all stdout in
  let errors = In_channel.input_all stderr in
  match Caml_unix.close_process_full (stdout, stdin, stderr) with
  | Caml_unix.WEXITED 0 -> output
  | _ -> failwithf "pandoc could not convert the math:\n%s" (String.rstrip errors) ()

let to_mathml formulas =
  run_pandoc (pandoc_input formulas)
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
    String.chop_prefix line ~prefix:"<p>" |> Option.bind ~f:(String.chop_suffix ~suffix:"</p>"))

let render formulas html =
  match formulas with
  | [] -> html
  | _ ->
    let mathml = to_mathml formulas in
    if List.length mathml <> List.length formulas then
      failwithf "pandoc returned %d formulas for %d" (List.length mathml) (List.length formulas) ();
    List.foldi mathml ~init:html ~f:(fun i html m ->
      String.substr_replace_first html ~pattern:(marker i) ~with_:m)
