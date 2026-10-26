(** Build-time math: [$...$], [$$...$$] and [```math] blocks become MathML,
    converted by pandoc. *)

type formula

(** [extract doc] replaces every math node in [doc] with a placeholder and
    returns the formulas in document order. *)
val extract : Cmarkit.Doc.t -> Cmarkit.Doc.t * formula list

(** [render formulas html] replaces the placeholders in [html] with MathML.
    Runs pandoc once, and only if there is math. Fails if pandoc is missing or
    rejects a formula. *)
val render : formula list -> string -> string
