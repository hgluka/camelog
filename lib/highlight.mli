(** Build-time syntax highlighting of markdown code blocks. *)

(** [grammars dir] is hilite's built-in grammars (OCaml, dune, opam, shell, diff)
    plus every [.json] TextMate grammar in [dir], if it exists. Fails on a
    grammar that doesn't parse. *)
val grammars : string -> TmLanguage.t

(** Highlights code blocks whose language matches a grammar's name, or else one
    of its file types. Other code blocks are left as they are. *)
val highlight : TmLanguage.t -> Cmarkit.Doc.t -> Cmarkit.Doc.t
