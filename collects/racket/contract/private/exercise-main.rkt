#lang racket/base

(require "exercise-base.rkt"
         "generate-base.rkt"
         "rand.rkt"
         "prop.rkt")

(provide contract-random-exercise)

(define (contract-random-exercise 
          ctc
          val
          #:fuel [fuel 5]
          #:print-gen [print-gen #f]
          #:tests [num-tests 1])
  (define (do-exercise)
    (add-trace (contract-struct-name ctc) 'exercise)
    (for ([i (in-range num-tests)])
         ((contract-struct-exercise ctc) val fuel print-gen)))
  (if (generate-env)
      (do-exercise)
      (parameterize ([generate-env (make-hash)])
        (do-exercise))))
