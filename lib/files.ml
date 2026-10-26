open Core
open Pages
open Templates

let create_dir input_dir output_dir p =
  (if not (String.is_empty output_dir)
   then (FileUtil.mkdir ~parent:true ~mode:(`Octal 0o755) (Filename.concat output_dir (relative_to input_dir p))))

let unpublished name = String.is_prefix (Filename.basename name) ~prefix:"_"

let rec traverse input_dir exclude apply_to_dir =
  let flat_contents = Sys_unix.ls_dir input_dir in
  let contents = List.map (List.filter flat_contents ~f:(fun p -> not (exclude p)))
      ~f:(fun p ->
        let full_path = Filename.concat input_dir p in
        match Sys_unix.is_directory full_path with
        | `Yes -> apply_to_dir full_path; traverse full_path exclude apply_to_dir
        | _ -> [full_path])
      in
  List.concat contents

let regular_file = fun filename ->
  match Sys_unix.is_file filename with
  | `Yes -> filename
  | `No -> failwithf "%s is not a regular file" filename ()
  | `Unknown -> failwithf "could not determine if %s is a regular file" filename ()

let process ~tm pages =
  let posts = list_of_posts pages is_post in
  List.iter pages
    ~f:(fun page ->
        match page.page_type with
        | Markdown template_path ->
          let page_contents = In_channel.read_all page.input_path in
          let template = In_channel.read_all (regular_file template_path) in
          let html =
            try md_to_html tm (escape_html page.title) page_contents template posts with
            | Failure msg -> failwithf "%s: %s" template_path msg () in
          Out_channel.write_all page.output_path ~data:(rebind_links_from page pages html)
        | Other -> FileUtil.cp [page.input_path] page.output_path)
