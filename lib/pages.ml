open Core

type page_type =
  | Markdown
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

let pages input_dir output_dir paths =
  List.map paths
    ~f:(fun path ->
        let path_and_ext = Filename.split_extension path in
        let ptype = match snd path_and_ext with
          | Some "md" -> Markdown
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
          relative_path = FilePath.add_extension cut_path new_ext;
          input_path = path;
          output_path = Filename.concat output_dir (FilePath.add_extension cut_path new_ext)
        })
