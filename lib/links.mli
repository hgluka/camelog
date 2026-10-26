(** Resolving [seek:] links. *)

(** [reroute src dest] is the relative link from page [src] to [dest]. *)
val reroute : string -> string -> string

(** [rebind_links_from page pages html] rewrites [href] and [src] attributes that
    start with [seek:] into links relative to [page]. [seek:/path] is an exact
    source path, [seek:name] a file name that must be unique. Targets are
    percent-decoded before lookup and the rewritten link is percent-encoded,
    so names with spaces or non-ASCII letters work. Fails if a link matches no
    file or several. *)
val rebind_links_from : Pages.page -> Pages.page list -> string -> string
