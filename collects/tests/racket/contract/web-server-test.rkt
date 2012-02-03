#lang racket/base

(require net/url
         racket/contract
         web-server/http)

(contract-add-generate url?
  (λ (fuel) (string->url "http://google.com")))

(contract-exercise-funs '(,make-binding) '(make-binding))

(contract-exercise-modules '(web-server/http)
                           ;; Enable to print values used for testing
                           ;#:print-gen (λ (v) (eprintf "Calling with value(s) ~s\n\n" v))
                           )
