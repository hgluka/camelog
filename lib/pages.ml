open Core

type page_type =
  | Markdown of string
  | Css
  | Other
  [@@deriving show]

type page = {
  page_type: page_type;
  title: string;
  date: string option;
  relative_path: string;
  input_path: string;
  output_path: string
} [@@deriving show]

let is_markdown pt =
  match pt with
  | Markdown _ -> true
  | _ -> false

let pages input_dir output_dir template paths =
  List.map paths
    ~f:(fun path ->
        let path_and_ext = Filename.split_extension path in
        let template_file = Filename.concat (Filename.dirname path) template in
        let ptype = match snd path_and_ext with
          | Some "md" -> Markdown template_file
          | Some "css" -> Css
          | _ -> Other in
        let new_ext = match snd path_and_ext with
          | Some "md" -> "html"
          | Some x -> x
          | None -> "" in
        let cut_path = String.chop_prefix_if_exists (fst path_and_ext) ~prefix:input_dir in
        let post_title = match List.find (In_channel.read_lines path) ~f:(fun l -> is_markdown ptype && (Stdlib.String.starts_with ~prefix:"##" l)) with
          | Some l -> String.sub l ~pos: 3 ~len:(String.length l - 3)
          | None -> Filename.basename cut_path in
        let date_re = Re.Pcre.re "\\d{4}-\\d{2}-\\d{2}" |> Re.compile in
        let date_str_opt = Option.map ~f:(fun s -> Re.Pcre.get_substring s 0) (Re.exec_opt date_re (Filename.basename cut_path)) in
        {
          page_type = ptype;
          title = post_title;
          date = date_str_opt;
          relative_path = String.sub (FilePath.add_extension cut_path new_ext) ~pos:1 ~len:(String.length (FilePath.add_extension cut_path new_ext) - 1);
          input_path = path;
          output_path = Filename.concat output_dir (FilePath.add_extension cut_path new_ext)
        })

let pretty_page_title page =
  match page.date with
  | Some d -> String.concat [d; ": "; page.title]
  | None -> page.title

let list_of_posts pages filter =
  String.concat ["<ul>\n    <li>"; String.concat ~sep:"</li>\n    <li>"
                   (List.filter_map pages ~f:(fun p -> if filter p
                                               then Some (String.concat ["<a href=\""; p.relative_path; "\">"; pretty_page_title p; "</a>"])
                                               else None));
                 "</li>\n  </ul>"]
