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
  let r = Cmarkit_html.renderer ~safe:false () in
  let content = Cmarkit_renderer.doc_to_string r (Cmarkit.Doc.of_string ~strict:false md) in
  interpolated template title content posts

let reroute src dest =
  let rec drop_common src_dirs dest_parts =
    match src_dirs, dest_parts with
    | s :: src_rest, d :: (_ :: _ as dest_rest) when String.equal s d -> drop_common src_rest dest_rest
    | _ -> src_dirs, dest_parts in
  let src_dirs, dest_parts = drop_common
      (List.tl_exn (Filename.parts (Filename.dirname src)))
      (List.tl_exn (Filename.parts dest)) in
  String.concat ~sep:"/" ((List.map ~f:(fun _ -> "..") src_dirs) @ dest_parts)

let output_name target =
  match Filename.split_extension target with
  | base, Some "md" -> base ^ ".html"
  | _ -> target

let seek page pages target =
  let matches =
    match String.chop_prefix target ~prefix:"/" with
    | Some path -> List.filter pages ~f:(fun p -> String.equal p.relative_path (output_name path))
    | None when String.mem target '/' ->
      failwithf "%s: seek:%s must be an exact path starting with / or a file name without /" page.input_path target ()
    | None -> List.filter pages ~f:(fun p -> String.equal (Filename.basename p.relative_path) (output_name target)) in
  match matches with
  | [found] -> found
  | [] -> failwithf "%s: seek:%s matches no file" page.input_path target ()
  | found -> failwithf "%s: seek:%s matches several files: %s" page.input_path target
               (String.concat ~sep:", " (List.map found ~f:(fun p -> p.relative_path))) ()

let link_regex = Re.Perl.re "(href|src)=\"(.*?)\"" |> Re.compile

let rebind_links_from page pages template =
  Re.replace link_regex ~f:(fun groups ->
    match String.chop_prefix (Re.Pcre.get_substring groups 2) ~prefix:"seek:" with
    | Some target ->
      String.concat [Re.Pcre.get_substring groups 1; "=\""; reroute page.relative_path (seek page pages target).relative_path; "\""]
    | None -> Re.Pcre.get_substring groups 0) template
