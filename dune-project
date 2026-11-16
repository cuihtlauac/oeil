(lang dune 3.17)

(name oeil)

(generate_opam_files true)

(source
 (github cuihtlauac/oeil))

(authors "Cuihtlauac Alvarado <cuihtlauac@tarides.com>")

(maintainers "Cuihtlauac Alvarado <cuihtlauac@tarides.com>")

(license ISC)

(package
 (name oeil)
 (synopsis "Terminal and MCP monitor for OCurrent pipelines")
 (description
  "oeil reads the web UI of an OCurrent service, such as deploy.ci.ocaml.org, and shows its pipeline in a terminal or to an MCP client. Its main use is to follow a merged pull request until it is deployed.")
 (depends
  (ocaml
   (>= 5.4))
  (console
   (>= 0.9.0))
  (console-eio
   (>= 0.9.0))
  (eio_main
   (>= 1.4))
  (cohttp-eio
   (>= 6.0))
  (tls-eio
   (>= 2.0))
  ca-certs
  mirage-crypto-rng
  xmlm
  (yojson
   (>= 2.0))
  (cmdliner
   (>= 1.3))
  uri
  ptime
  logs
  fmt
  (alcotest :with-test)))
