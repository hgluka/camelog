(lang dune 3.17)

(name camelog)

(generate_opam_files true)

(source
 (github hgluka/camelog))

(authors "Luka Hadzi-Djokic <luka.hadzi@proton.me>")

(maintainers "Luka Hadzi-Djokic <luka.hadzi@proton.me>")

(license MIT)

(documentation https://github.com/hgluka/camelog/blob/main/README.md)

(package
 (name camelog)
 (synopsis "A static blog generator written in OCaml") 
 (depends
   ocaml
   core
   ppx_jane
   fileutils
   cmarkit
   re))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
