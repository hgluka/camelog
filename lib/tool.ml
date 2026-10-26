open Core

let run ?(input = "") ~missing prog args =
  let stdout, stdin, stderr =
    try Caml_unix.open_process_args_full prog (Array.of_list (prog :: args)) (Caml_unix.environment ()) with
    | Caml_unix.Unix_error (Caml_unix.ENOENT, _, _) -> failwith missing in
  Out_channel.output_string stdin input;
  Out_channel.close stdin;
  let output = In_channel.input_all stdout in
  let errors = In_channel.input_all stderr in
  match Caml_unix.close_process_full (stdout, stdin, stderr) with
  | Caml_unix.WEXITED 0 -> Ok output
  | _ -> Error errors
