(** Filling templates. *)

(** [interpolated template title content posts] replaces [inscribe:title],
    [inscribe:content] and [inscribe:posts] in [template]. Fails on any other
    [inscribe:] name. *)
val interpolated : string -> string -> string -> string -> string

(** Escapes [&], [<], [>] and double quotes for HTML. *)
val escape_html : string -> string

(** [list_of_posts pages filter] is an HTML list of [seek:] links to the pages
    that pass [filter], newest first. *)
val list_of_posts : Pages.page list -> (Pages.page -> bool) -> string
