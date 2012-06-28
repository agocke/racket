#lang racket/base

(require racket/sequence
         "rand.rkt")

(provide filter-vals
         generate-env
         make-env
         env-add
         bulk-env-add)

; env parameter
(define generate-env (make-parameter #f))

(define make-env make-hash)

; Adds a new contract and value to the environment if they don't already exist
(define (env-add env ctc val)
  (unless (void? val)
    (let ([curvals (hash-ref env ctc (hash))])
      (unless (hash-has-key? curvals val)
        (hash-set! env 
                   ctc
                   (hash-set curvals val 0))))))

; Bulk add to the environment
(define (bulk-env-add ctcs vals [env (make-env)])
  (for ([c ctcs]
        [v vals])
      (env-add env c v))
  env)

;; Helper function for getting random matching values from the environment.
;; "Matching" is defined as returning true in the predicate f, which has the
;; contract (ctc (listof any) -> boolean?). Returns a sequence of the matching
;; (ctc . fun) pairs. If no values match the sequence is empty.
(define (filter-vals f env)
  (for/fold ([vals-seq empty-sequence])
            ([(ctc vals) (in-hash env)]
             #:when (f ctc vals))
    (sequence-append vals-seq 
                     (sequence-map (λ (v) (cons ctc v))
                                   (in-hash-keys vals)))))
