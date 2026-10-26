open Core
open Pages

let process ~tm ~preamble pages =
  let posts = Templates.list_of_posts pages is_post in
  let templates = Hashtbl.create (module String) in
  List.iter pages
    ~f:(fun page ->
        match page.page_type with
        | Markdown template_path ->
          let page_contents = In_channel.read_all page.input_path in
          let template = Hashtbl.find_or_add templates template_path
              ~default:(fun () -> In_channel.read_all (Files.regular_file template_path)) in
          let content =
            try Markdown.render tm preamble page_contents with
            | Failure msg -> failwithf "%s: %s" page.input_path msg () in
          let html =
            try Templates.interpolated template (Templates.escape_html page.title) content posts with
            | Failure msg -> failwithf "%s: %s" template_path msg () in
          Out_channel.write_all page.output_path ~data:(Links.rebind_links_from page pages html)
        | Other -> FileUtil.cp [page.input_path] page.output_path)

let run ~verbose input_dir output_dir =
  let input_dir = String.rstrip input_dir ~drop:(Char.equal '/') in
  let tm = Highlight.grammars (Filename.concat input_dir "_grammars") in
  let preamble = Tikz.preamble (Filename.concat input_dir "_tikz.tex") in
  Files.create_dir input_dir output_dir input_dir;
  let site = pages input_dir output_dir (Files.traverse input_dir Files.unpublished (Files.create_dir input_dir output_dir)) in
  if verbose then List.iter site ~f:(fun page -> print_s (sexp_of_page page));
  process ~tm ~preamble site
