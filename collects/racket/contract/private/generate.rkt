#lang racket/base

(require "rand.rkt"
         "env.rkt"
         "exercise-base.rkt"
         "exercise-main.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "prop.rkt"
         racket/list)

(provide generate-env
         env-stash
         get-env-contracts
         gen-fail-map

         contract-add-generate

         contract-random-generate
         generate/direct
         
         generate-ctc-fail
         generate-ctc-fail?
         generate-ctc-fail-ctc)


; Exported for unit testing only
(provide filter-vals
         find-val-fun
         generate/indirect-env
         valid-gen)

; intercept parameter
; See env-stash for intercept usage
(define intercept-env (make-parameter #f))

; Adds a new contract and value to the environment if they don't already exist.
; If intercept-env is a vector containing a contract in the first index then
; env-stash will _also_ set the second and third indices to the ctc and val if
; the ctc is equal or stronger than the contract in the vector
(define (env-stash env maybe-ctc val)
  (define ctc (coerce-contract 'env-stash maybe-ctc))
  (let ([ienv (intercept-env)])
    (when (and (vector? ienv)
               (contract-stronger? ctc (vector-ref ienv 0)))
      (begin (vector-set! ienv 1 ctc)
             (vector-set! ienv 2 val))))
  (env-add env ctc val))


; Iterates through generation methods until failure. Returns
; generate-ctc-fail if no value could be generated
(define (contract-random-generate maybe-ctc fuel)
  (define ctc (coerce-contract 'contract-random-generate maybe-ctc))
  (add-trace (contract-struct-name ctc) 'contract-random-generate)
  (parameterize ([generate-env (or (generate-env) (make-env))])
    ; choose randomly until one method succeeds or all fail
    (let trygen ([options (permute (list generate/direct
                                         generate/direct-env
                                         generate/indirect-env))])
      (if (null? options)
          (generate-ctc-fail ctc)
          (let ([val ((car options) ctc fuel)])
            (if (generate-ctc-fail? val)
                (trygen (cdr options))
                val))))))

; generate/direct :: contract int -> value for contract
; Attempts to make a generator that generates values for this contract
; directly. Returns generate-ctc-fail if making a generator fails.
(define (generate/direct maybe-ctc fuel)
  (define ctc (coerce-contract 'generate/direct maybe-ctc))
  (add-trace (contract-struct-name ctc) 'generate/direct)
  (parameterize ([generate-env (or (generate-env) (make-env))])
    (if (> fuel 0)
        ((contract-struct-generate ctc) fuel)
        (generate-ctc-fail ctc))))

; generate/direct-env :: contract int -> value
; Attemps to find a value with the given contract in the environment.
; Returns it if found and generate-ctc-fail otherwise.
(define (generate/direct-env maybe-ctc fuel)
  (define ctc (coerce-contract 'generate-direct/env maybe-ctc))
  (add-trace (contract-struct-name ctc) 'generate-direct/env)
  (parameterize ([generate-env (or (generate-env) (make-env))])
    (let ([val (find-val (λ (c vs) (contract-stronger? c ctc))
                          (generate-env))])
      (if (void? val)
          (generate-ctc-fail ctc)
          (cdr val)))))

;; generate/indirect-env :: contract int -> (int -> value for contract)
;; Attempts to make a generator that generates values for this contract
;; by calling functions in the environment. Note that only procedures 
;; as values in the environment will be considered.
(define (generate/indirect-env maybe-ctc fuel)
  (define ctc (coerce-contract 'generate/indirect-env maybe-ctc))
  (add-trace (contract-struct-name ctc) 'generate/indirect-env)
  (define (get-permuted-vals ctc)
    (permute-sequence (filter-vals (valid-gen ctc)
                                   (generate-env))))
  (define (get-a-val ctc options)
    (for/or ([rand-ctc+fun options])
      (let ([gen (grab-generated-val ctc 
                                     rand-ctc+fun 
                                     fuel)])
        (if (not (generate-ctc-fail? gen))
            (list gen)
            #f))))
  (parameterize ([generate-env (or (generate-env) (make-env))])
    (if (> fuel 0)
        (let* ([vals (get-permuted-vals ctc)]
               [val (get-a-val ctc vals)])
          (if (not val)
              (generate-ctc-fail ctc)
              (car val)))
        (generate-ctc-fail ctc))))

;; Exercises the given (ctc . fun) pair in order to grab a value
;; generated during the exercising. Returns either the value generated
;; or generate-ctc-fail.
(define (grab-generated-val ctc rand-ctc+fun fuel)
  (parameterize ([intercept-env (make-vector 3)])
    (let* ([ienv (intercept-env)]
           [fctc (car rand-ctc+fun)]
           [fun (cdr rand-ctc+fun)]
           [handler (λ (exn) (generate-ctc-fail fctc))])
      (begin (vector-set! ienv 0 ctc)
             (with-handlers ([exn:fail? handler])
               (contract-random-exercise fctc
                                         fun
                                         #:fuel fuel))
             (let ([got-ctc (vector-ref ienv 1)])
               (if (not (number? got-ctc))
                 (vector-ref ienv 2)
                 (generate-ctc-fail ctc)))))))

(define find-val (compose rand-seq filter-vals))

(define (find-val-fun ctc env)
  (find-val (valid-gen ctc) env))

;; valid-gen makes a predicate for filter-vals which returns #t 
;; if the environment procedures can generate the target contract.
(define ((valid-gen target-ctc) env-ctc env-vals)
  (and (not (= 0 (hash-count env-vals)))
       ; Check all contracts that env-ctc to see if any is
       ; stronger than the target contract
       (for/or ([c ((contract-struct-can-generate env-ctc) 'exercise)])
         (contract-stronger? c target-ctc))))

