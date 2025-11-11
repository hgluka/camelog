open Core

(* Setup *)
let directory_contents =
  ["test_website/template.html";
   "test_website/posts/template.html";
   "test_website/posts/2025-11-11-post-two.md";
   "test_website/posts/2025-11-11-post-one.md";
   "test_website/index.md"]

let testable_page = Alcotest.testable Util.Pages.pp_page Util.Pages.equal_page

let page_list : Util.Pages.page list =
  [{ page_type = Util.Pages.Other; title = "template";
     date = None; relative_path = "template.html";
     input_path = "test_website/template.html";
     output_path = "output_dir/template.html" };
   { page_type = Util.Pages.Other; title = "template";
     date = None; relative_path = "posts/template.html";
     input_path = "test_website/posts/template.html";
     output_path = "output_dir/posts/template.html" };
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
    <li><a href="posts/template.html">template</a></li>
    <li><a href="posts/2025-11-11-post-two.html">2025-11-11: Post Two</a></li>
    <li><a href="posts/2025-11-11-post-one.html">2025-11-11: Post One</a></li>
  </ul>|}

(* Util.Files tests *)
let test_regular_file_success () =
  Alcotest.(check string) "same filename" "test_website/index.md" (Util.Files.regular_file "test_website/index.md")

let test_regular_file_fail () =
  Alcotest.check_raises "same failure" (Failure "Not a regular file") (fun _ -> ignore (Util.Files.regular_file "missing.ml"))

let test_traverse () =
  Alcotest.(check (list string)) "same directory content list"
    directory_contents (Util.Files.traverse "test_website" (fun _ -> false) (fun _ -> ()))

(* Util.Templates tests *)
let test_reroute () =
  Alcotest.(check string) "same filename" "../index.md" (Util.Templates.reroute "posts/2025-11-11-post-one.md" "index.md")

let test_interpolate () =
  Alcotest.(check string) "same string" "test TEST_TITLE; TEST_CONTENT; TEST_POSTS; [other]"
    (Util.Templates.interpolated "test [title]; [content]; [posts]; [other]" "TEST_TITLE" "TEST_CONTENT" "TEST_POSTS")

(* Util.Pages tests *)
let test_pages () =
  Alcotest.(check (list testable_page)) "same page list" page_list (Util.Pages.pages "test_website" "output_dir" "template.html" directory_contents)

let test_post_list () =
  Alcotest.(check string) "same string" post_list (Util.Pages.list_of_posts page_list
                                                  (fun f -> List.mem (Filename.parts f.relative_path) "posts" ~equal:String.equal))

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
      test_case "Interpolation test" `Quick test_interpolate;
    ];
    "Util.Pages", [
      test_case "Pages test" `Quick test_pages;
      test_case "Post list test" `Quick test_post_list;
    ]
  ]
