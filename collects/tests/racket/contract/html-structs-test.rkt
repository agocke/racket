#lang racket/base

(require racket/contract
         xml/private/structures
         html/html-structs
         html/sgml-reader
         html)

(contract-random-generate kid-lister/c 10)

; Define xml-specific generators
;; Infinite loop in gen-read-sgml
(contract-exercise-modules '(xml/private/structures
                              html/html-structs
                              html/html-spec
                              html/sgml-reader
                              html
                              ))
