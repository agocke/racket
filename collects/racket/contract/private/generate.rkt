#lang racket/base

(require "rand.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "prop.rkt"
         racket/list
         racket/sequence)

(provide generate-env
         env-stash
         get-env-contracts
         gen-fail-map

         contract-add-generate
         contract-random-generate

         generate/direct
         generate/choose
         
         generate-ctc-fail
         generate-ctc-fail?
         generate-ctc-fail-ctc)


; Exported for unit testing only
(provide filter-vals
         find-val-fun
         generate/indirect-env
         make-env-from-funs
         valid-gen)

; generate : contract int -> ctc value or error
(define (contract-random-generate ctc [fuel 5] #:funs [funs #f]
         #:fail [fail 
                  (λ () 
                     (error 'contract-random-generate
                            "Unable to construct any generator for contract: ~s"
                            (contract-struct-name 
                              (coerce-contract 'contract-random-generate 
                                               ctc))))])
  (let ([def-ctc (coerce-contract 'contract-random-generate ctc)])
    (parameterize ([generate-env (if funs
                                   (apply make-env-from-funs funs)
                                   (make-hash))])
      ; choose randomly
      (let ([val (generate/choose def-ctc fuel)])
        (if (generate-ctc-fail? val)
            (fail)
            val)))))

; intercept parameter
; See env-stash for intercept usage
(define intercept-env (make-parameter #f))

; Adds a new contract and value to the environment if they don't already exist.
; If intercept-env is a vector containing a contract in the first index then
; env-stash will _also_ set the second and third indices to the ctc and val if
; the ctc is equal or stronger than the contract in the vector
(define (env-stash env ctc val)
  (let ([ienv (intercept-env)])
    (when (and (vector? ienv)
               (contract-stronger? ctc (vector-ref ienv 0)))
      (begin (vector-set! ienv 1 ctc)
             (vector-set! ienv 2 val))))
  (let ([curvals (hash-ref env ctc (list))])
    (hash-set! env ctc (cons val curvals))))


; Iterates through generation methods until failure. Returns
; generate-ctc-fail if no value could be generated
(define (generate/choose ctc fuel)
  (eprintf "generate/choose ~s\n" (contract-struct-name ctc))
  ; choose randomly until one method succeeds or all fail
  (let trygen ([options (permute (list generate/direct
                                       generate/direct-env
                                       generate/indirect-env))])
    (if (null? options)
        (generate-ctc-fail ctc)
        (let ([val ((car options) ctc fuel)])
          (if (generate-ctc-fail? val)
              (trygen (cdr options))
              val)))))

; generate/direct :: contract int -> value for contract
; Attempts to make a generator that generates values for this contract
; directly. Returns generate-ctc-fail if making a generator fails.
(define (generate/direct ctc fuel)
  (eprintf "generate/direct ~s\n" (contract-struct-name ctc))
  (let ([direct-trace (generate/direct-trace)])
    (when direct-trace
      (let ([name (contract-struct-name ctc)])
        (hash-update! direct-trace
                      name
                      (λ (i) (+ i 1))
                      0))))
  (let* ([ctc (coerce-contract 'generate/direct ctc)]
         [g (contract-struct-generate ctc)])
    ; Check if the contract has a direct generate attached
    (if (generate-ctc-fail? g)
        g 
        (g fuel))))

; generate/direct-env :: contract int -> value
; Attemps to find a value with the given contract in the environment.
; Returns it if found and generate-ctc-fail otherwise.
(define (generate/direct-env ctc fuel)
  (eprintf "generate/direct-env ~s\n" (contract-struct-name ctc))
  (let ([env-trace (generate/env-trace)])
    (when env-trace
      (let ([name (contract-struct-name ctc)])
        (hash-update! env-trace
                      name
                      (λ (i) (+ i 1))
                      0))))
  (let* ([ctc (coerce-contract 'generate-direct/env ctc)]
         [val (find-val (λ (c vs) (contract-stronger? c ctc))
                        (generate-env))])
    (if (void? val)
        (generate-ctc-fail ctc)
        (cdr val))))

;; generate/indirect-env :: contract int -> (int -> value for contract)
;; Attempts to make a generator that generates values for this contract
;; by calling functions in the environment. Note that only procedures 
;; as values in the environment will be considered.
(define (generate/indirect-env ctc fuel)
  (eprintf "generate/indirect-env ~s\n" (contract-struct-name ctc))
  (let ([indirect-trace (generate/indirect-trace)])
    (when indirect-trace
      (let ([name (contract-struct-name ctc)])
        (hash-update! indirect-trace
                      name
                      (λ (i) (+ i 1))
                      0))))
  (let ([ctc (coerce-contract 'generate/indirect-env ctc)]
        [fail (generate-ctc-fail ctc)])
    (if (> fuel 0)
        (let* ([vals (permute-sequence (filter-vals (valid-gen ctc)
                                                    (generate-env)))]
               [val (for/or ([rand-ctc+fun vals])
                      (let ([gen (grab-generated-val ctc 
                                                     rand-ctc+fun 
                                                     fuel)])
                        (if (not (generate-ctc-fail? gen))
                            (list gen)
                            #f)))])
          (if (not val)
              fail
              (car val)))
        fail)))

;; Helper function for getting random matching values from the environment.
;; "Matching" is defined as returning true in the predicate f, which has the
;; contract (ctc (listof any) -> boolean?). Returns a sequence of the matching
;; (ctc . fun) pairs. If no values match the sequence is empty.
(define (filter-vals f env)
  (for/fold ([vals-seq empty-sequence])
            ([(ctc vals) (in-hash env)]
             #:when (f ctc vals))
    (sequence-append vals-seq (map (λ (v) (cons ctc v))
                                   vals))))

(define find-val (compose rand-seq filter-vals))

;; valid-gen makes a predicate for filter-vals which returns #t 
;; if the environment procedures can generate the target contract.
(define (valid-gen target-ctc)
  (λ (env-ctc env-vals)
     (and (not (empty? env-vals))
          (procedure? (car env-vals))
          ; Check all contracts that env-ctc to see if any is
          ; stronger than the target contract
          (for/or ([c ((contract-struct-can-generate env-ctc) 'exercise)])
                  (contract-stronger? c target-ctc)))))

(define (find-val-fun ctc env)
  (find-val (valid-gen ctc) env))

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
               ((contract-struct-exercise fctc) fun fuel #f))
             (let ([got-ctc (vector-ref ienv 1)])
               (if (not (number? got-ctc))
                 (vector-ref ienv 2)
                 (generate-ctc-fail ctc)))))))

;; make-env-from-funs :: (listof procedure?) -> env
;; Make an environment from a list of functions
(define make-env-from-funs 
  (λ funs
     (let ([env (make-hash)])
       (for-each (λ (f) (when (has-contract? f) 
                              (env-stash env (value-contract f) f)))
                 funs)
       env)))
