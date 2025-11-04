open Core
open Pages
open Templates

let create_dir output_dir p =
  (if not (String.is_empty output_dir)
   then (FileUtil.mkdir ~parent:true ~mode:(`Octal 0o755) (Filename.concat output_dir p)))

let rec traverse input_dir exclude apply_to_dir =
  let flat_contents = Sys_unix.ls_dir input_dir in
  let contents = List.map (List.filter flat_contents ~f:(fun p -> not (exclude p)))
      ~f:(fun p ->
        let full_path = Filename.concat input_dir p in
        match Sys_unix.is_directory full_path with
        | `Yes -> apply_to_dir p; traverse full_path exclude apply_to_dir
        | _ -> [full_path])
      in
  List.concat contents

let regular_file = fun filename ->
  match Sys_unix.is_file filename with
  | `Yes -> filename
  | `No -> failwith "Not a regular file"
  | `Unknown -> failwith "Could not determine if file is regular"

let process pages template_path =
  let template = In_channel.with_file template_path ~f:(fun ic ->
            In_channel.input_all ic) in
  let posts = list_of_posts pages (fun p -> Option.is_some (String.substr_index ~pattern:"posts/" p.relative_path)) in
  List.map pages
    ~f:(fun page ->
        let page_contents = In_channel.with_file page.input_path ~f:(fun ic ->
            In_channel.input_all ic) in
        match page.page_type with
        | Markdown ->
          Out_channel.write_all page.output_path ~data:(md_to_html page.title page_contents (rebind_links_from page pages template) (rebind_links_from page pages posts))
        | _ -> Out_channel.write_all page.output_path ~data:page_contents)
