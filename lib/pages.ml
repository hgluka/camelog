open Core

type page_type =
  | Markdown of string
  | Css
  | Other
  [@@deriving show]

type page = {
  page_type: page_type;
  title: string;
  relative_path: string;
  input_path: string;
  output_path: string
} [@@deriving show]

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
        {
          page_type = ptype;
          title = FilePath.basename cut_path;
          relative_path = String.sub (FilePath.add_extension cut_path new_ext) ~pos:1 ~len:(String.length (FilePath.add_extension cut_path new_ext) - 1);
          input_path = path;
          output_path = Filename.concat output_dir (FilePath.add_extension cut_path new_ext)
        })

let list_of_posts pages filter =
  String.concat ["<ul>\n    <li>"; String.concat ~sep:"</li>\n    <li>" (List.filter_map pages ~f:(fun p -> if filter p then Some (String.concat ["<a href=\""; p.relative_path; "\">"; p.title; "</a>"]) else None)); "</li>\n  </ul>"]
