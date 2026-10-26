(** HTML comments that stand in for content converted after rendering. *)

(** [marker kind i] is the placeholder for the [i]th item of [kind] on a page. *)
val marker : string -> int -> string

(** [fill kind values html] replaces each [marker kind i] in [html] with the
    [i]th of [values]. *)
val fill : string -> string list -> string -> string
