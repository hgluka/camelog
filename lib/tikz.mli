(** Build-time TikZ: [```tikz] blocks become inline SVG, compiled with latex
    and dvisvgm. *)

(** [preamble path] is the contents of [path] if it exists, otherwise a
    standalone preamble loading [tikz], [tikz-cd] and the [arrows.meta],
    [positioning] and [automata] libraries. *)
val preamble : string -> string

(** [extract doc] replaces every [```tikz] block in [doc] with a placeholder
    and returns the figures in document order. *)
val extract : Cmarkit.Doc.t -> Cmarkit.Doc.t * string list

(** [render preamble figures html] compiles each figure with [preamble] and
    replaces its placeholder in [html] with the SVG. Black becomes
    [currentColor], so figures follow the text colour. Fails with LaTeX's error
    lines if a figure doesn't compile. *)
val render : string -> string list -> string -> string
