(lang dune 3.3)

(name lab1)

(generate_opam_files true)

(authors "PPL2022 Your Name")
(maintainers "Your Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lab1)
 (synopsis "Write youself a lambda calculus.")
 (description "Implement lambda calculus and play with it.")
 (depends (ocaml (>= 4.14.0)) dune base ppxlib)
 (tags
  (lambda)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
