(lang dune 3.24)

(name oasm)

(generate_opam_files true)

(source
 (github lostbymidnight/OAsm))

(authors "Midnight <151787330+lostbymidnight@users.noreply.github.com>")

(maintainers "Midnight <151787330+lostbymidnight@users.noreply.github.com>")

(license GPL-3.0-only)

(package
 (name oasm)
 (synopsis "An interpreter for an 8-bit assembly like language.")
 (description
  "An 8-bit assembly-like language interpreter with syntax derived from standard 8-bit assembly.")
 (depends
  (ocaml
   (>= 4.14))
  dune)
 (tags
  (assembly interpreter vm)))
