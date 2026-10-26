let render tm preamble md =
  let r = Cmarkit_html.renderer ~safe:false () in
  let doc, figures = Tikz.extract (Cmarkit.Doc.of_string ~strict:false md) in
  let doc, formulas = Math.extract (Highlight.highlight tm doc) in
  Cmarkit_renderer.doc_to_string r doc
  |> Math.render formulas
  |> Tikz.render preamble figures
