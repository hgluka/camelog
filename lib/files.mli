(** Reading the input directory and writing the site. *)

(** [create_dir input_dir output_dir dir] creates the output directory matching [dir]. *)
val create_dir : string -> string -> string -> unit

(** [traverse dir exclude apply_to_dir] lists the files under [dir], skipping names
    for which [exclude] holds and calling [apply_to_dir] on each subdirectory. *)
val traverse : string -> (string -> bool) -> (string -> unit) -> string list

(** Returns the path if it is a regular file, fails otherwise. *)
val regular_file : string -> string

(** Renders markdown pages into their templates and copies everything else. *)
val process : Pages.page list -> unit
