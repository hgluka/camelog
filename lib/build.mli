(** Building a site. *)

(** [run ~verbose input_dir output_dir] renders every markdown page in
    [input_dir] into [output_dir] and copies everything else, skipping
    unpublished names. With [verbose], prints each page first. *)
val run : verbose:bool -> string -> string -> unit
