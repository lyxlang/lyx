(lang dune 3.13)

(using menhir 3.0)

(name lyx)

(generate_opam_files)

(source
 (github lyxlang/lyx))

(maintainers "Aljebriq <143266740+aljebriq@users.noreply.github.com>")

(authors "Aljebriq <143266740+aljebriq@users.noreply.github.com>")

(license GPL-3.0-only)

(package
 (name lyx)
 (synopsis "The Lyx purely functional programming language")
 (description
  "Lyx is a purely functional language with an elegant and practical syntax.")
 (depends
  (ocaml
   (>= 5))
  sedlex
  menhir
  menhirLib
  ppx_deriving
  uuseg
  yojson)
 (tags
  (ml fp pl)))
