#lang racket/base

(require racket/contract)

(define (test-contract-generation ctc [monkey-with values] #:size [size 10])
  ;; generator : number[of tests] number[size bound] ??[env] -> any
  (define generator (contract-struct-generator ctc))
  (define an-example (generator 10 size '()))
  (monkey-with (contract ctc an-example 'pos 'neg)))
  
(test-contract-generation (listof integer?))
(test-contract-generation (between/c 1 100))
;(test-contract-generation (flat-contract char?))
(test-contract-generation (>=/c 0))
(test-contract-generation (<=/c 0))
(test-contract-generation (>/c 0))
(test-contract-generation (string-len/c 10))
(test-contract-generation (vector-immutableof negative?))
coerce-contract
