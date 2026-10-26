open Core
open Pages

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

let percent_decode s =
  let buf = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then
      match s.[i] with
      | '%' when i + 2 < String.length s && Char.is_hex_digit s.[i + 1] && Char.is_hex_digit s.[i + 2] ->
        Buffer.add_char buf (Char.of_int_exn (Int.of_string ("0x" ^ String.sub s ~pos:(i + 1) ~len:2)));
        go (i + 3)
      | c -> Buffer.add_char buf c; go (i + 1) in
  go 0;
  Buffer.contents buf

let percent_encode s =
  String.concat_map s ~f:(fun c ->
    if Char.is_alphanum c || String.mem "-._~/" c then String.of_char c
    else sprintf "%%%02X" (Char.to_int c))

let link_regex = Re.Perl.re "(href|src)=\"(.*?)\"" |> Re.compile

let rebind_links_from page pages html =
  Re.replace link_regex ~f:(fun groups ->
    match String.chop_prefix (Re.Pcre.get_substring groups 2) ~prefix:"seek:" with
    | Some target ->
      let found = seek page pages (percent_decode target) in
      String.concat [Re.Pcre.get_substring groups 1; "=\""; percent_encode (reroute page.relative_path found.relative_path); "\""]
    | None -> Re.Pcre.get_substring groups 0) html
