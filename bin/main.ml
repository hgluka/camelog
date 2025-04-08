open Core

let transmute (title : string) (filename : string) (template:string) : string =
  let md_contents = In_channel.with_file filename ~f:(fun ic ->
    In_channel.input_all ic) in
  let template_contents = In_channel.with_file template ~f:(fun ic ->
    In_channel.input_all ic) in
  List.iter (Util.traverse "test/assets" (fun _ -> false))
  ~f:(print_endline);
  Util.md_to_html title md_contents template_contents

let command =
  Command.basic
    ~summary:"A static blog generator written in OCaml"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command
      title =
        anon ("title" %: string)
      and filename =
        anon ("filename" %: Command.Arg_type.create Util.regular_file)
      and template =
        anon ("template" %: Command.Arg_type.create Util.regular_file)
     in
     fun () -> print_endline (transmute title filename template))

let () = Command_unix.run ~version:"1.0" command
