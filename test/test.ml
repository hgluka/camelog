(* Util.Files tests *)
let test_regular_file_success () =
  Alcotest.(check string) "same filename" "test_website/index.md" (Util.Files.regular_file "test_website/index.md")

let test_regular_file_fail () =
  Alcotest.check_raises "same failure" (Failure "Not a regular file") (fun _ -> ignore (Util.Files.regular_file "missing.ml"))

let test_traverse () =
  Alcotest.(check (list string)) "same directory content list"
    ["test_website/template.html";
     "test_website/posts/template.html";
     "test_website/posts/2025-11-11-post-two.md";
     "test_website/posts/2025-11-11-post-one.md";
     "test_website/index.md"] (Util.Files.traverse "test_website" (fun _ -> false) (fun _ -> ()))

(* Util.Templates tests *)
let test_reroute () =
  Alcotest.(check string) "same filename" "../index.md" (Util.Templates.reroute "posts/2025-11-11-post-one.md" "index.md")

let test_interpolate () =
  Alcotest.(check string) "same string" "test TEST_TITLE; TEST_CONTENT; TEST_POSTS; [other]"
    (Util.Templates.interpolated "test [title]; [content]; [posts]; [other]" "TEST_TITLE" "TEST_CONTENT" "TEST_POSTS")

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
    ]
  ]
