open Core

let marker kind i = sprintf "<!--camelog-%s-%d-->" kind i

let fill kind values html =
  List.foldi values ~init:html ~f:(fun i html value ->
    String.substr_replace_first html ~pattern:(marker kind i) ~with_:value)
