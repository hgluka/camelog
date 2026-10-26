(** Paths and the input directory. *)

(** [relative_to input_dir path] is [path] without the [input_dir] prefix and leading [/]. *)
val relative_to : string -> string -> string

(** [create_dir input_dir output_dir dir] creates the output directory matching [dir]. *)
val create_dir : string -> string -> string -> unit

(** Names starting with [_] are never rendered or copied: templates, [_drafts],
    [_grammars] and anything else that should stay out of the site. *)
val unpublished : string -> bool

(** [traverse dir exclude apply_to_dir] lists the files under [dir], skipping names
    for which [exclude] holds and calling [apply_to_dir] on each subdirectory. *)
val traverse : string -> (string -> bool) -> (string -> unit) -> string list

(** Returns the path if it is a regular file, fails otherwise. *)
val regular_file : string -> string
