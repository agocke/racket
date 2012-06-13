#lang racket/base

(require "exercise-base.rkt"
         "rand.rkt"
         "prop.rkt")

(provide contract-random-exercise)

(define (contract-random-exercise 
          ctc
          val
          fuel
          print-gen
          #:tests [num-tests 1])
  (add-trace (contract-struct-name ctc) 'exercise)
  (for ([i (in-range num-tests)])
       ((contract-struct-exercise ctc) val fuel print-gen)))
