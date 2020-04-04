(lang dune 2.0)
(generate_opam_files true)

(name chibibb)
(version 0.0.1)

(authors "Bob Poekert")
(license MIT)
(maintainers "bob@poekert.com")

(package
    (name chibibb)
    (synopsis "Lightweight bulletin board server")
    (depends
        (dune (>= 2))
        (ocaml (>= 4.08.0))

        (opium (>= v0.12.2))

        (tyxml (>= 4.3.0))

        (logs (>= 0.7.0))

        (bigstringaf (>= 0.6.1))
        (capnp (>= 3.4.0))
        (crc (>= 2.0.0))

        (utop :dev)
        (merlin :dev)
        (ocamlformat :dev)
        (ocp-indent :dev)))
