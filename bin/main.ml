open Core
open Util.Files
open Util.Pages

let transmute input_dir output_dir =
  let site = pages input_dir output_dir (traverse input_dir (fun path -> String.equal (Filename.basename path) "mould.html") (create_dir output_dir)) in
  ignore (List.map site ~f:(fun page -> print_endline (show_page page)));
  ignore (process site)

let command =
  Command.basic
    ~summary:"A static blog generator written in OCaml"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command
      input_dir =
        anon ("input_dir" %: string)
      and output_dir =
        anon ("output_dir" %: string)
     in
     fun () -> transmute input_dir output_dir)

let () = Command_unix.run ~version:"1.0" command
