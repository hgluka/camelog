open Core

type page_type =
  | Markdown of string
  | Other
  [@@deriving show, eq]

type page = {
  page_type: page_type;
  title: string;
  date: string option;
  relative_path: string;
  input_path: string;
  output_path: string
} [@@deriving show, eq]

let is_markdown pt =
  match pt with
  | Markdown _ -> true
  | _ -> false

let relative_to input_dir path =
  String.chop_prefix_if_exists path ~prefix:input_dir
  |> String.chop_prefix_if_exists ~prefix:"/"

let date_re = Re.Pcre.re "\\d{4}-\\d{2}-\\d{2}" |> Re.compile

let pages input_dir output_dir template paths =
  List.map paths
    ~f:(fun path ->
        let path_and_ext = Filename.split_extension path in
        let template_file = Filename.concat (Filename.dirname path) template in
        let ptype = match snd path_and_ext with
          | Some "md" -> Markdown template_file
          | _ -> Other in
        let new_ext = match snd path_and_ext with
          | Some "md" -> "html"
          | Some x -> x
          | None -> "" in
        let cut_path = relative_to input_dir (fst path_and_ext) in
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

let escape_html s =
  let buf = Buffer.create (String.length s) in
  Cmarkit_html.buffer_add_html_escaped_string buf s;
  Buffer.contents buf

let pretty_page_title page =
  match page.date with
  | Some d -> String.concat [d; ": "; page.title]
  | None -> page.title

let compare_posts p q =
  match Option.compare String.compare q.date p.date with
  | 0 -> String.compare q.relative_path p.relative_path
  | c -> c

let list_of_posts pages filter =
  String.concat ["<ul>\n    <li>"; String.concat ~sep:"</li>\n    <li>"
                   (List.filter_map (List.sort pages ~compare:compare_posts) ~f:(fun p -> if filter p
                                               then Some (String.concat ["<a href=\"seek:/"; p.relative_path; "\">"; escape_html (pretty_page_title p); "</a>"])
                                               else None));
                 "</li>\n  </ul>"]
