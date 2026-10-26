(** Pages of the site and the post list. *)

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

(** [relative_to input_dir path] is [path] without the [input_dir] prefix and leading [/]. *)
val relative_to : string -> string -> string

(** [pages input_dir output_dir template paths] describes each file in [paths].
    Markdown files use the file named [template] in their own directory. *)
val pages : string -> string -> string -> string list -> page list

(** A markdown page under [posts/]. *)
val is_post : page -> bool

(** Escapes [&], [<], [>] and double quotes for HTML. *)
val escape_html : string -> string

(** [list_of_posts pages filter] is an HTML list of [seek:] links to the pages
    that pass [filter], newest first. *)
val list_of_posts : page list -> (page -> bool) -> string
