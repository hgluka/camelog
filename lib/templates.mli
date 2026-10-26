(** Filling templates and resolving [seek:] links. *)

(** [interpolated template title content posts] replaces [inscribe:title],
    [inscribe:content] and [inscribe:posts] in [template]. Fails on any other
    [inscribe:] name. *)
val interpolated : string -> string -> string -> string -> string

(** [render tm preamble md] renders markdown to HTML, highlighting code blocks
    with the grammars in [tm], converting math to MathML and compiling TikZ
    figures with [preamble]. *)
val render : TmLanguage.t -> string -> string -> string

(** [reroute src dest] is the relative link from page [src] to [dest]. *)
val reroute : string -> string -> string

(** [rebind_links_from page pages html] rewrites [href] and [src] attributes that
    start with [seek:] into links relative to [page]. [seek:/path] is an exact
    source path, [seek:name] a file name that must be unique. Fails if a link
    matches no file or several. *)
val rebind_links_from : Pages.page -> Pages.page list -> string -> string
