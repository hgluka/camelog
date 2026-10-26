(** Running external programs. *)

(** [run ?input ~missing prog args] runs [prog] from [PATH] with [args], feeding
    it [input]. Returns its output, or its error output if it exits with a
    non-zero status. Fails with [missing] if [prog] isn't installed. *)
val run : ?input:string -> missing:string -> string -> string list -> (string, string) result
