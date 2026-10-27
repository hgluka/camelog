open Core

let default_preamble = {|\documentclass{article}
\pagestyle{empty}
\def\pgfsysdriver{pgfsys-dvisvgm.def}
\usepackage{tikz}
\usepackage{tikz-cd}
\usetikzlibrary{arrows.meta,positioning,automata}
|}

let preamble path =
  match Sys_unix.is_file path with
  | `Yes -> In_channel.read_all path
  | `No | `Unknown -> default_preamble

let extract doc =
  let figures = ref [] in
  let block _ = function
    | Cmarkit.Block.Code_block (code, meta) ->
      (match Option.bind (Cmarkit.Block.Code_block.info_string code) ~f:(fun (info, _) ->
         Cmarkit.Block.Code_block.language_of_info_string info) with
       | Some ("tikz", _) ->
         let tex = Cmarkit.Block.Code_block.code code |> List.map ~f:Cmarkit.Block_line.to_string |> String.concat ~sep:"\n" in
         figures := tex :: !figures;
         let m = Placeholder.marker "tikz" (List.length !figures - 1) in
         Cmarkit.Mapper.ret (Cmarkit.Block.Html_block (Cmarkit.Block_line.list_of_string m, meta))
       | _ -> Cmarkit.Mapper.default)
    | _ -> Cmarkit.Mapper.default in
  let doc = Cmarkit.Mapper.map_doc (Cmarkit.Mapper.make ~block ()) doc in
  doc, List.rev !figures

let latex_errors log =
  String.split_lines log
  |> List.filter ~f:(fun line -> String.is_prefix line ~prefix:"!" || String.is_prefix line ~prefix:"l.")
  |> String.concat ~sep:"\n"

let svg_header = Re.Perl.re "<\\?xml[^>]*>\\n?|<!--[^>]*-->\\n?" |> Re.compile

let svg_ids = Re.Perl.re "(id='|href='#|url\\(#)" |> Re.compile

let inline_svg prefix svg =
  Re.replace_string svg_header ~by:"" svg
  |> Re.replace svg_ids ~f:(fun groups -> Re.Group.get groups 1 ^ prefix)

let compile preamble i tex =
  let dir = Stdlib.Filename.temp_dir "camelog" "tikz" in
  let file name = Filename.concat dir name in
  Exn.protect ~finally:(fun () -> FileUtil.rm ~recurse:true [dir]) ~f:(fun () ->
    Out_channel.write_all (file "figure.tex")
      ~data:(String.concat [preamble; "\\begin{document}\n"; tex; "\n\\end{document}\n"]);
    (match Tool.run ~missing:"TikZ figures need latex, which was not found on PATH" "latex"
             ["-interaction=nonstopmode"; "-halt-on-error"; "-output-directory=" ^ dir; file "figure.tex"] with
     | Ok _ -> ()
     | Error _ -> failwithf "LaTeX could not compile a TikZ figure:\n%s" (latex_errors (In_channel.read_all (file "figure.log"))) ());
    match Tool.run ~missing:"TikZ figures need dvisvgm, which was not found on PATH" "dvisvgm"
            ["--no-fonts"; "--currentcolor"; "--exact-bbox"; "--stdout"; file "figure.dvi"] with
    | Ok svg -> String.concat ["<div class=\"tikz\">"; inline_svg (sprintf "tikz%d-" i) svg; "</div>"]
    | Error errors -> failwithf "dvisvgm could not convert a TikZ figure:\n%s" (String.rstrip errors) ())

let render preamble figures html =
  Placeholder.fill "tikz" (List.mapi figures ~f:(compile preamble)) html
