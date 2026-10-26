open Core

(* Setup *)
let directory_contents =
  ["test_website/template.html";
   "test_website/styles.css";
   "test_website/posts/template.html";
   "test_website/posts/diagram.svg";
   "test_website/posts/2025-11-12-post-three.md";
   "test_website/posts/2025-11-11-post-two.md";
   "test_website/posts/2025-11-11-post-one.md";
   "test_website/index.md"]

let testable_page = Alcotest.testable Util.Pages.pp_page Util.Pages.equal_page

let page_list : Util.Pages.page list =
  [{ page_type = Util.Pages.Other; title = "template";
     date = None; relative_path = "template.html";
     input_path = "test_website/template.html";
     output_path = "output_dir/template.html" };
   { page_type = Util.Pages.Other; title = "styles";
     date = None; relative_path = "styles.css";
     input_path = "test_website/styles.css";
     output_path = "output_dir/styles.css" };
   { page_type = Util.Pages.Other; title = "template";
     date = None; relative_path = "posts/template.html";
     input_path = "test_website/posts/template.html";
     output_path = "output_dir/posts/template.html" };
   { page_type = Util.Pages.Other; title = "diagram";
     date = None; relative_path = "posts/diagram.svg";
     input_path = "test_website/posts/diagram.svg";
     output_path = "output_dir/posts/diagram.svg" };
   { page_type =
       (Util.Pages.Markdown "test_website/posts/template.html");
     title = "Post Three & <Friends>"; date = (Some "2025-11-12");
     relative_path = "posts/2025-11-12-post-three.html";
     input_path = "test_website/posts/2025-11-12-post-three.md";
     output_path = "output_dir/posts/2025-11-12-post-three.html" };
   { page_type =
       (Util.Pages.Markdown "test_website/posts/template.html");
     title = "Post Two"; date = (Some "2025-11-11");
     relative_path = "posts/2025-11-11-post-two.html";
     input_path = "test_website/posts/2025-11-11-post-two.md";
     output_path = "output_dir/posts/2025-11-11-post-two.html" };
   { page_type =
       (Util.Pages.Markdown "test_website/posts/template.html");
     title = "Post One"; date = (Some "2025-11-11");
     relative_path = "posts/2025-11-11-post-one.html";
     input_path = "test_website/posts/2025-11-11-post-one.md";
     output_path = "output_dir/posts/2025-11-11-post-one.html" };
   { page_type =
       (Util.Pages.Markdown "test_website/template.html");
     title = "index"; date = None; relative_path = "index.html";
     input_path = "test_website/index.md";
     output_path = "output_dir/index.html" }]

let post_list = {|<ul>
    <li><a href="seek:/posts/2025-11-12-post-three.html">2025-11-12: Post Three &amp; &lt;Friends&gt;</a></li>
    <li><a href="seek:/posts/2025-11-11-post-two.html">2025-11-11: Post Two</a></li>
    <li><a href="seek:/posts/2025-11-11-post-one.html">2025-11-11: Post One</a></li>
  </ul>|}

(* Util.Files tests *)
let test_regular_file_success () =
  Alcotest.(check string) "same filename" "test_website/index.md" (Util.Files.regular_file "test_website/index.md")

let test_regular_file_fail () =
  Alcotest.check_raises "same failure" (Failure "missing.ml is not a regular file") (fun _ -> ignore (Util.Files.regular_file "missing.ml"))

let test_traverse () =
  Alcotest.(check (list string)) "same directory content list"
    directory_contents (List.sort (Util.Files.traverse "test_website" (fun _ -> false) (fun _ -> ())) ~compare:String.descending)

(* Util.Templates tests *)
let test_reroute () =
  Alcotest.(check string) "same filename" "../index.md" (Util.Templates.reroute "posts/2025-11-11-post-one.md" "index.md")

let test_reroute_same_dir () =
  Alcotest.(check string) "same filename" "2025-11-11-post-two.html"
    (Util.Templates.reroute "posts/2025-11-11-post-one.html" "posts/2025-11-11-post-two.html")

let test_reroute_subdir () =
  Alcotest.(check string) "same filename" "posts/2025-11-11-post-two.html"
    (Util.Templates.reroute "index.html" "posts/2025-11-11-post-two.html")

let post_one = List.find_exn page_list ~f:(fun p -> String.equal p.relative_path "posts/2025-11-11-post-one.html")

let rebind template = Util.Templates.rebind_links_from post_one page_list template

let test_seek_path () =
  Alcotest.(check string) "same string" {|<a href="../index.html">|} (rebind {|<a href="seek:/index.md">|})

let test_seek_name () =
  Alcotest.(check string) "same string" {|<link href="../styles.css">|} (rebind {|<link href="seek:styles.css">|})

let test_seek_leaves_other_links () =
  let template = {|<a href="https://example.com/index.html"><img src="index.html">|} in
  Alcotest.(check string) "same string" template (rebind template)

let test_seek_missing () =
  Alcotest.check_raises "same failure"
    (Failure "test_website/posts/2025-11-11-post-one.md: seek:missing.css matches no file")
    (fun () -> ignore (rebind {|<link href="seek:missing.css">|}))

let test_seek_ambiguous () =
  Alcotest.check_raises "same failure"
    (Failure "test_website/posts/2025-11-11-post-one.md: seek:template.html matches several files: template.html, posts/template.html")
    (fun () -> ignore (rebind {|<a href="seek:template.html">|}))

let test_seek_relative_path () =
  Alcotest.check_raises "same failure"
    (Failure "test_website/posts/2025-11-11-post-one.md: seek:posts/x.md must be an exact path starting with / or a file name without /")
    (fun () -> ignore (rebind {|<a href="seek:posts/x.md">|}))

let test_interpolate () =
  Alcotest.(check string) "same string" "test TEST_TITLE; TEST_CONTENT; TEST_POSTS; [other]"
    (Util.Templates.interpolated "test [title]; [content]; [posts]; [other]" "TEST_TITLE" "TEST_CONTENT" "TEST_POSTS")

(* Util.Pages tests *)
let check_pages input_dir =
  Alcotest.(check (list testable_page)) "same page list"
    page_list
    (List.sort
       (Util.Pages.pages input_dir "output_dir" "template.html" directory_contents)
       ~compare:(fun p q -> String.descending p.relative_path q.relative_path))

let test_pages () = check_pages "test_website"

let test_pages_trailing_slash () = check_pages "test_website/"

let test_post_list () =
  Alcotest.(check string) "same string" post_list (Util.Pages.list_of_posts page_list Util.Pages.is_post)

let () =
  let open Alcotest in
  run "Util" [
    "Util.Files", [
      test_case "Regular file test positive" `Quick test_regular_file_success;
      test_case "Regular file test negative" `Quick test_regular_file_fail;
      test_case "Directory traversal test" `Quick test_traverse;
    ];
    "Util.Templates", [
      test_case "Reroute test" `Quick test_reroute;
      test_case "Reroute same directory test" `Quick test_reroute_same_dir;
      test_case "Reroute subdirectory test" `Quick test_reroute_subdir;
      test_case "Seek exact path test" `Quick test_seek_path;
      test_case "Seek by name test" `Quick test_seek_name;
      test_case "Seek leaves other links test" `Quick test_seek_leaves_other_links;
      test_case "Seek missing file test" `Quick test_seek_missing;
      test_case "Seek ambiguous name test" `Quick test_seek_ambiguous;
      test_case "Seek relative path test" `Quick test_seek_relative_path;
      test_case "Interpolation test" `Quick test_interpolate;
    ];
    "Util.Pages", [
      test_case "Pages test" `Quick test_pages;
      test_case "Pages trailing slash test" `Quick test_pages_trailing_slash;
      test_case "Post list test" `Quick test_post_list;
    ]
  ]
