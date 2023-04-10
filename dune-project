(lang dune 3.5)

(name DoomMetal)
(authors "Tazouev Arbi" "Guinet Virgile")
(maintainers "Tazouev Arbi" "Guinet Virgile")
(using menhir 2.1)
(cram enable)
(package
 (name DoomMetal)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune tsdl sdl2)
 (tags
  ("Doom" "Ocaml" "Wolfenstein" "Raycasting")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
