#lang racket/base

(require racket/contract)

(define (test-contract-generation ctc [monkey-with values] #:size [size 10])
  ;; generator : number[of tests] number[size bound] ??[env] -> any
  (define example-vals (contract-generate ctc size))
  (monkey-with (contract ctc example-vals 'pos 'neg)))
  
(begin0
  (test-contract-generation integer?)
  (test-contract-generation exact-nonnegative-integer?)
  (test-contract-generation boolean?)
  (test-contract-generation char?)
  (test-contract-generation (between/c 1 100))
  (test-contract-generation (listof integer?))
  (test-contract-generation (>=/c 0))
  (test-contract-generation (<=/c 0))
  (test-contract-generation (>/c 0))
  (test-contract-generation (</c 0))
  ;(test-contract-generation (string-len/c 10))
  ;(test-contract-generation (vector-immutableof negative?))
  ((test-contract-generation (-> integer? integer?)) 1)
  ((test-contract-generation (-> (-> integer? integer?) boolean?)) +)
  (display "All tests passed"))
