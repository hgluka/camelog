open Core

type formula = { tex: string; display: bool }

let extract doc =
  let formulas = ref [] in
  let add formula =
    formulas := formula :: !formulas;
    Placeholder.marker "math" (List.length !formulas - 1) in
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
  match Tool.run ~input ~missing:"math needs pandoc, which was not found on PATH" "pandoc"
          ["--from=markdown"; "--to=html"; "--mathml"; "--wrap=none"; "--fail-if-warnings"] with
  | Ok output -> output
  | Error errors -> failwithf "pandoc could not convert the math:\n%s" (String.rstrip errors) ()

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
    Placeholder.fill "math" mathml html
