#lang racket/base

(require net/url
         racket/contract
         web-server/http)

(contract-add-generate url?
  (λ (fuel) (string->url "http://google.com")))
(contract-add-generate output-port? (λ (fuel) (open-output-file "/dev/null" #:exists 'update)))

(contract-exercise-modules '(web-server/http)
                           ;; Enable to print values used for testing
                           ;#:print-gen (λ (v) (eprintf "Calling with value(s) ~s\n\n" v))
                           )
