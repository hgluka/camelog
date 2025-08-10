open Core

type page_type =
  | Markdown
  | Css
  | Other

type page = {
  page_type: page_type;
  title: string;
  relative_path: string;
  input_path: string;
  output_path: string
}

let string_of_ptype pt = match pt with
  | Markdown -> "Markdown"
  | Css -> "Css"
  | Other -> "Other"

let string_of_page p =
  String.concat ["page: { title = \""; p.title; "\", relative_path = "; p.relative_path; ", type = "; string_of_ptype p.page_type; " }"]

let pages (input_dir: string) (output_dir: string) (paths : string list) : page list =
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

let rec traverse input_dir exclude output_dir =
  let flat_contents = Sys_unix.ls_dir input_dir in
  let contents = List.map (List.filter flat_contents ~f:(fun p -> not (exclude p)))
      ~f:(fun p ->
        let full_path = Filename.concat input_dir p in
        match Sys_unix.is_directory full_path with
        | `Yes -> (if not (String.is_empty output_dir)
                   then (FileUtil.mkdir ~parent:true ~mode:(`Octal 0o755) (Filename.concat output_dir p) )); traverse full_path exclude output_dir
        | _ -> [full_path])
      in
  List.concat contents

let regular_file = fun filename ->
  match Sys_unix.is_file filename with
  | `Yes -> filename
  | `No -> failwith "Not a regular file"
  | `Unknown -> failwith "Could not determine if file is regular"

let md_to_html (title:string) (md:string) (template:string) : string =
  let doc = Cmarkit.Doc.of_string md in
  let html_template = Scanf.format_from_string template "%a %a" in
  let r = Cmarkit_html.renderer ~safe:true () in
  let buffer_add_doc = Cmarkit_renderer.buffer_add_doc r in
  let buffer_add_title = Cmarkit_html.buffer_add_html_escaped_string in
  Printf.kbprintf Buffer.contents (Buffer.create 1024)
    html_template buffer_add_title title buffer_add_doc doc

let process (pages: page list) (template_path: string) =
  let template = In_channel.with_file template_path ~f:(fun ic ->
            In_channel.input_all ic) in
  List.map pages
    ~f:(fun page ->
        let page_contents = In_channel.with_file page.input_path ~f:(fun ic ->
            In_channel.input_all ic) in
        match page.page_type with
        | Markdown -> Out_channel.write_all page.output_path ~data:(md_to_html page.title page_contents template)
        | _ -> Out_channel.write_all page.output_path ~data:page_contents)

let transmute template input_dir exclude output_dir =
  ignore (process (pages input_dir output_dir (traverse input_dir exclude output_dir)) template)
