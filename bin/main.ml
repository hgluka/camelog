open Core

let command =
  Command.basic
    ~summary:"A static blog generator written in OCaml"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command
      template =
        anon ("template" %: string)
      and input_dir =
        anon ("input_dir" %: string)
      and output_dir =
        anon ("output_dir" %: string)
     in
     fun () -> Util.transmute template input_dir (fun _s -> false) output_dir)

let () = Command_unix.run ~version:"1.0" command
