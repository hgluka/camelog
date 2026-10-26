open Core
open Pages

let templater = Re.Pcre.re "inscribe:([A-Za-z_]+)" |> Re.compile

let interpolated template title content posts =
  Re.replace templater ~all:true ~f:(fun groups ->
    match Re.Pcre.get_substring groups 1 with
    | "title" -> title
    | "content" -> content
    | "posts" -> posts
    | name -> failwithf "inscribe:%s is not a placeholder, use inscribe:title, inscribe:content or inscribe:posts" name ()) template

let escape_html s =
  let buf = Buffer.create (String.length s) in
  Cmarkit_html.buffer_add_html_escaped_string buf s;
  Buffer.contents buf

let pretty_page_title page =
  match page.date with
  | Some d -> String.concat [d; ": "; page.title]
  | None -> page.title

let list_of_posts pages filter =
  String.concat ["<ul>\n    <li>"; String.concat ~sep:"</li>\n    <li>"
                   (List.filter_map (List.sort pages ~compare:compare_posts) ~f:(fun p -> if filter p
                                               then Some (String.concat ["<a href=\"seek:/"; p.relative_path; "\">"; escape_html (pretty_page_title p); "</a>"])
                                               else None));
                 "</li>\n  </ul>"]
