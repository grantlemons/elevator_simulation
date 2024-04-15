(lang dune 3.14)

(name elevator_simulator)

(generate_opam_files true)

; (source
;  (github username/reponame))

(authors "Grant Lemons")

(maintainers "Grant Lemons")

(license AGPL-3.0-only)

(package
 (name elevator_simulator)
 (synopsis "Discrete Event Simulation for a system of elevators.")
 (description "Discrete Event Simulation for a system of elevators. Created for the purpose of learning DES, not as an actual product.")
 (depends ocaml dune (alcotest :with-test))
 (tags
  (topics "simulation" "des")))
