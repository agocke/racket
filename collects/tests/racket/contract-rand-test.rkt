#lang racket/base

(require racket/contract)

(define (test-contract-generation ctc [monkey-with values] #:size [size 10])
  ;; generator : number[of tests] number[size bound] ??[env] -> any
  (define example-vals (contract-generate ctc size))
  (monkey-with (contract ctc example-vals 'pos 'neg)))
  
(test-contract-generation integer?)
(test-contract-generation (between/c 1 100))
(test-contract-generation (listof integer?))
;(test-contract-generation (flat-contract char?))
(test-contract-generation (>=/c 0))
(test-contract-generation (<=/c 0))
;(test-contract-generation (>/c 0))
;(test-contract-generation (string-len/c 10))
;(test-contract-generation (vector-immutableof negative?))
(test-contract-generation (-> integer? boolean?))
