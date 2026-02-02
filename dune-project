(lang dune 3.0)
(name off_switch)

(package
 (name off_switch)
 (synopsis "Hardware off-switch with cryptographic authorization for AI accelerators")
 (depends
  (ocaml (>= 4.14))
  base
  hardcaml
  zarith
  ppx_jane
  ppx_hardcaml))
