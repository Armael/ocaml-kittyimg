(lang dune 2.7)
(generate_opam_files true)

(name kittyimg)
(version 0.1)
(source (github Armael/ocaml-kittyimg))
(authors "Armaël Guéneau <armael.gueneau@ens-lyon.org>")
(maintainers "Armaël Guéneau <armael.gueneau@ens-lyon.org>")
(license MIT)

(package
 (name kittyimg)
 (synopsis "An implementation of Kitty's terminal graphics protocol")
 (description "Kitty's terminal graphics protocol allows displaying images in compatible terminal emulators")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 2.0))
  (base64 (>= 3.0.0))
 ))
