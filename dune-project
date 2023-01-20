(lang dune 3.0)

(name DoomMetal)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Tazouev Arbi & Guinet Virgile")

(maintainers "Tazouev Arbi")

(license LICENSE)

(documentation https://url/to/documentation)


(package
 (name DoomMetal)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune bogue)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
