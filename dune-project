(lang dune 3.22)

(name uplotgui)

(generate_opam_files true)

(source
 (github tatjam/uplotgui))

(authors "Tatjam <tatjam02@gmail.com>")

(maintainers "Tatjam <tatjam02@gmail.com>")

(license MIT)

;; Use a standard identifier like MIT, BSD-3-Clause, or GPL-3.0

(package
 (name uplotgui)
 (synopsis "youplot but using GNUPlot for GUI")
 (description "youplot but using GNUPlot for GUI")
 (depends ocaml dune ppx_deriving))
