(** The pages of the site. *)

type page_type =
  | Markdown of string  (** Markdown source, rendered with the template at this path *)
  | Other  (** Copied to the output as is *)
  [@@deriving sexp_of, equal]

type page = {
  page_type: page_type;
  title: string;  (** First [##] heading, or the file name without extension *)
  date: string option;  (** [YYYY-MM-DD] from the file name *)
  relative_path: string;  (** Output path relative to the output directory *)
  input_path: string;
  output_path: string
} [@@deriving sexp_of, equal]

(** [pages input_dir output_dir paths] describes each file in [paths].
    Markdown files use the [_template.html] in their own directory. *)
val pages : string -> string -> string list -> page list

(** A markdown page under [posts/]. *)
val is_post : page -> bool

(** Orders posts newest first, undated pages last. *)
val compare_posts : page -> page -> int
