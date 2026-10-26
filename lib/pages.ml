open Core

type page_type =
  | Markdown of string
  | Other
  [@@deriving sexp_of, equal]

type page = {
  page_type: page_type;
  title: string;
  date: string option;
  relative_path: string;
  input_path: string;
  output_path: string
} [@@deriving sexp_of, equal]

let is_markdown pt =
  match pt with
  | Markdown _ -> true
  | _ -> false

let date_re = Re.Pcre.re "\\d{4}-\\d{2}-\\d{2}" |> Re.compile

let pages input_dir output_dir paths =
  List.map paths
    ~f:(fun path ->
        let path_and_ext = Filename.split_extension path in
        let template_file = Filename.concat (Filename.dirname path) "_template.html" in
        let ptype = match snd path_and_ext with
          | Some "md" -> Markdown template_file
          | _ -> Other in
        let new_ext = match snd path_and_ext with
          | Some "md" -> "html"
          | Some x -> x
          | None -> "" in
        let cut_path = Files.relative_to input_dir (fst path_and_ext) in
        let relative_path = FilePath.add_extension cut_path new_ext in
        let heading = match ptype with
          | Markdown _ -> List.find (In_channel.read_lines path) ~f:(String.is_prefix ~prefix:"##")
          | Other -> None in
        let post_title = match heading with
          | Some l -> String.lstrip l ~drop:(Char.equal '#') |> String.strip
          | None -> Filename.basename cut_path in
        let date_str_opt = Option.map ~f:(fun s -> Re.Pcre.get_substring s 0) (Re.exec_opt date_re (Filename.basename cut_path)) in
        {
          page_type = ptype;
          title = post_title;
          date = date_str_opt;
          relative_path;
          input_path = path;
          output_path = Filename.concat output_dir relative_path
        })

let is_post page =
  is_markdown page.page_type &&
  match Filename.parts page.relative_path with
  | "." :: "posts" :: _ -> true
  | _ -> false

let compare_posts p q =
  match Option.compare String.compare q.date p.date with
  | 0 -> String.compare q.relative_path p.relative_path
  | c -> c
