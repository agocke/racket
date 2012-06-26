#lang racket/base

(require racket/contract
         racket/set
         net/url)

(contract-add-generate url?
  (λ (fuel) (string->url "http://google.com")))

(contract-exercise-modules '(net/url)
                           #:exclude '(display-pure-port get-pure-port/headers)
                           #:tests 200)
