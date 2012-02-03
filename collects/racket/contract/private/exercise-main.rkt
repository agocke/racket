#lang racket/base

(require "exercise-base.rkt"
         "prop.rkt")

(provide contract-random-exercise)

(define (contract-random-exercise 
          ctc
          val
          fuel
          print-gen
          #:tests [num-tests 1])
  (when (exercise-logging)
    (eprintf "contract-random-exercise ~a\n" (contract-struct-name ctc)))
  (let ([ex-trace (exercise-trace)])
    (when ex-trace
      (let ([name (contract-struct-name ctc)])
        (hash-update! ex-trace
                      name
                      (λ (i) (+ i 1))
                      0))))
  (for ([i (in-range num-tests)])
       ((contract-struct-exercise ctc) val fuel print-gen)))
