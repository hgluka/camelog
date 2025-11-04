open Core
open Pages

let interpolated template title content posts =
  let templater = Re.Pcre.re "\\[(title|content|posts)\\]" |> Re.compile in
  Re.replace templater ~all:true ~f:(fun groups ->
    match Re.Pcre.get_substring groups 1 with
    | "title" -> title
    | "content" -> content
    | "posts" -> posts
    | _ -> "") template

let md_to_html title md template posts =
  let r = Cmarkit_html.renderer ~safe:true () in
  let content = Cmarkit_renderer.doc_to_string r (Cmarkit.Doc.of_string md) in
  interpolated template title content posts

let reroute src dest =
  let src_list = List.tl_exn (Filename.parts (Filename.dirname src)) in
  let dest_list = List.tl_exn (Filename.parts dest) in
  print_endline (String.concat ["src: "; src]);
  print_endline (String.concat ["dest: "; dest]);
  String.concat ~sep:"/" ((List.map ~f:(fun _ -> "..") src_list) @ dest_list)

let rebind_links_from page pages template =
  let link_regex = Re.Perl.re "href=\"(.*?)\"" |> Re.compile in
  Re.replace link_regex ~f:(fun groups ->
    let link_in_template = Re.Pcre.get_substring groups 1 in
    print_endline (String.concat ["link: "; link_in_template]);
    match (List.find pages ~f:(fun p -> String.equal (Filename.basename p.relative_path) (Filename.basename link_in_template))) with
    | Some x -> String.concat ["href=\""; (reroute page.relative_path x.relative_path); "\""]
    | None -> Re.Pcre.get_substring groups 0) template
