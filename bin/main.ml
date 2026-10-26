open Core
open Camelog.Files
open Camelog.Pages

let transmute ~verbose input_dir template output_dir =
  let input_dir = String.rstrip input_dir ~drop:(Char.equal '/') in
  create_dir input_dir output_dir input_dir;
  let site = pages input_dir output_dir template (traverse input_dir (fun path ->
      let filename = Filename.basename path in
      String.equal filename template || String.is_prefix filename ~prefix:"WIP") (create_dir input_dir output_dir)) in
  if verbose then List.iter site ~f:(fun page -> print_s (sexp_of_page page));
  process site

let command =
  Command.basic
    ~summary:"A static blog generator written in OCaml"
    ~readme:(fun () -> {|Renders every .md file in INPUT_DIR into OUTPUT_DIR as .html, using the
file named TEMPLATE in the same directory, and copies everything else.
Files starting with WIP are skipped.

Template placeholders: inscribe:title, inscribe:content, inscribe:posts
Links: seek:/path/from/input_dir or seek:unique-file-name|})
    (let%map_open.Command
      input_dir =
        anon ("input_dir" %: string)
      and template =
        anon ("template" %: string)
      and output_dir =
        anon ("output_dir" %: string)
      and verbose =
        flag "-verbose" no_arg ~doc:" print every page before building the site"
     in
     fun () ->
       try transmute ~verbose input_dir template output_dir with
       | Failure msg | Sys_error msg -> eprintf "camelog: %s\n" msg; Stdlib.exit 1)

let () = Command_unix.run ~version:"1.0" command
