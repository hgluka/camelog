(** Rendering markdown. *)

(** [render tm preamble md] renders markdown to HTML, highlighting code blocks
    with the grammars in [tm], converting math to MathML and compiling TikZ
    figures with [preamble]. *)
val render : TmLanguage.t -> string -> string -> string
