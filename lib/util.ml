open Core

type page_type =
  | Markdown
  | Css
  | Other

type page = {
  title: string;
  full_path: string;
  relative_path: string
}

let pages (paths : string list) : page list =
  List.map paths
    ~f:(fun path -> {
          title = (Filename.basename path);
          full_path = Caml_unix.realpath path;
          relative_path = path
        })

let rec traverse directory exclude =
  let flat_contents = Sys_unix.ls_dir directory in
  let contents = List.map (List.filter flat_contents ~f:(fun p -> not (exclude p)))
      ~f:(fun p ->
        let full_path = Filename.concat directory p in
        match Sys_unix.is_directory full_path with
        | `Yes -> traverse full_path exclude
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
