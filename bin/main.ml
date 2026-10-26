open Core

let command =
  Command.basic
    ~summary:"A static blog generator written in OCaml"
    ~readme:(fun () -> {|Renders every .md file in INPUT_DIR into OUTPUT_DIR as .html, using the
_template.html in the same directory, and copies everything else.
Names starting with _ are skipped. TextMate grammars in INPUT_DIR/_grammars
are used to highlight code blocks.

Template placeholders: inscribe:title, inscribe:content, inscribe:posts
Links: seek:/path/from/input_dir or seek:unique-file-name|})
    (let%map_open.Command
      input_dir =
        anon ("input_dir" %: string)
      and output_dir =
        anon ("output_dir" %: string)
      and verbose =
        flag "-verbose" no_arg ~doc:" print every page before building the site"
     in
     fun () ->
       try Camelog.Build.run ~verbose input_dir output_dir with
       | Failure msg | Sys_error msg -> eprintf "camelog: %s\n" msg; Stdlib.exit 1)

let () = Command_unix.run ~version:"1.0" command
