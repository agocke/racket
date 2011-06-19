#lang racket/base

(require "rand.rkt"
         "generate-base.rkt"
         "guts.rkt"
         racket/list)

(provide ;use-env
         env-stash

         contract-generate

         check-ctc-val

         generate/direct
         generate/choose

         make-generate-ctc-fail
         generate-ctc-fail?)

; env parameter
(define generate-env (make-parameter #f))

; Adds a new contract and value to the environment if
; they don't already exist
(define (env-stash ctc val)
  (let* ([curvals (hash-ref (generate-env) ctc (list))])
    (hash-set! (generate-env) ctc (cons val curvals))))

;; hash tables
;(define freq-hash (make-hash))
;(define gen-hash (make-hash))

;; thread-cell
;(define arg-names-count (make-thread-cell 0))

;; generate integer? 
(add-generate integer?
           (λ (fuel)
             (rand-choice
              [1/10 0]
              [1/10 1]
              [1/10 -1]
              [1/10 2147483647]
              [1/10 -2147483648]
              [3/10 (- 100 (rand 200))]
              [else (- 1000000000 (rand 2000000000))])))

(add-generate exact-nonnegative-integer?
               (λ (fuel)
                 (abs ((find-generate integer?) fuel))))


(add-generate positive?
           (λ (fuel)
             (rand-choice
              [1/10 1]
              [1/10 1/3]
              [1/10 0.12]
              [1/10 2147483647]
              [else 4])))

(add-generate boolean?
    (λ (fuel)
       (rand-choice
         [1/2 #t]
         [else #f])))
;           (λ (fuel)
;             (define (boolean?-static fuel)
;               (rand-choice
;                [1/2 #t]
;                [else #f]))
;             
;             (rand-choice
;              [2/3 (boolean?-static fuel)]
;              [else (let-values ([(res v) (use-env fuel generate-env boolean?)])
;                      (if res
;                          v
;                          (boolean?-static fuel generate-env)))])))



;; TODO: add new field to each contract. Move the code to guts
;; The set of functions beelow needs to be moved into guts.rkt
;; A new property for every contract has to be added with a name of
;; can-generate-given (or some better name). The interface looks like this:
;; can-generate-given: val -> (list-of ((list-of in-ctc), out-ctc, lambda-from-in-ctc-to-out-ctc))
;; in other words given a val with a contract we get the contract of this value. Than we ;; get the implementation of can-generate-given from the contract property and using it 
;; we generate a list of tuples. Each tuple shows one possible use of this val in generating 
;; out-ctc give a number of in-ctc. The last element in the tuple is something that can
;; be used to generate the right output given the right input.



;(define (gen-opts have-val want-ctc have-ctc fuel env)
;  (append (if (contract-stronger? have-ctc want-ctc)
;              (list have-val)
;              '())
;          (if (base->? have-ctc)
;              (let* ([gens (map contract-struct-generate
;                                (base->-doms/c have-ctc))])
;                (if (member #f gens)
;                    '()
;                    (let useful-results ([result-ctcs (base->-rngs/c have-ctc)]
;                                         [i 0])
;                      (if (empty? result-ctcs)
;                          '()
;                          (let* ([args (map (λ (g)
;                                              ; what should n-tests and size be
;                                              (g 0 0 env))
;                                            gens)])
;                            (append (gen-opts (λ ()
;                                                (call-with-values (λ ()
;                                                                    (apply have-val args))
;                                                                  (λ args
;                                                                    (list-ref args i)))) 
;                                              want-ctc 
;                                              (first result-ctcs)
;                                              fuel
;                                              env)
;                                    (useful-results (rest result-ctcs) (+ i 1))))))))
;                '())))

;(define (use-env fuel ctc
;                 #:test [is-test #f])
;  (let ([options (flatten (map (λ (e-i)
;                                 ;; contact-stronger? stronger weaker -> #
;                                 (gen-opts (env-item-name e-i)
;                                           ctc
;                                           (env-item-ctc e-i)
;                                           fuel
;                                           (generate-env)))
;                               (generate-env)))])
;    
;    (if (not (null? options))
;        (values #t (if is-test
;                       options
;                       ((list-ref options (rand (length options))))))
;        (values #f #f))))


;(define (generate ctc env)
;  (let ([g (contract-struct-generate ctc)]
;        [e (let-values ([(res f) (use-env 0 0 env ctc)])
;             res)])
;    (if (or g e)
;        (λ (n-tests size)
;          (rand-choice
;           [1/2 (g n-tests size env)]
;           [else (let-values ([(res v) (use-env n-tests size env ctc)])
;                   v)]))
;        #f)))

; generate : contract int -> ctc value or error
(define (contract-generate ctc fuel)
 (let ([def-ctc (coerce-contract 'contract-generate ctc)])
   (parameterize ([generate-env (make-hash)])
     ; choose randomly
     (let ([val (generate/choose def-ctc fuel)])
       (if (generate-ctc-fail? val)
         (error 'contract-generate
                "Unable to construct any generator for contract: ~e"
                ctc)
         val)))))

; Iterates through generation methods until failure. Returns
; generate-ctc-fail if no value could be generated
(define (generate/choose ctc fuel)
 (let ([options (permute (list generate/direct
                               generate/direct-env
                               ))])
   ; choose randomly
   (let trygen ([options options])
     (if (empty? options)
       (make-generate-ctc-fail)
       (let* ([option (car options)]
              [val (option ctc fuel)])
         (if (generate-ctc-fail? val)
           (trygen (cdr options))
                  val))))))

; generate/direct :: contract int -> (int -> value for contract)
; Attempts to make a generator that generates values for this contract
; directly. Returns generate-ctc-fail if making a generator fails.
(define (generate/direct ctc fuel)
  (let ([g (contract-struct-generate ctc)])
    ; Check if the contract has a direct generate attached
    (if (generate-ctc-fail? g)
      ; Everything failed -- we can't directly generate this ctc
      g 
      (g fuel))))

(define (generate/direct-env ctc fuel)
  ; TODO: find out how to make negative test cases
  (env-stash (coerce-contract 'char? char?) #\a)
  (let* ([keys (hash-keys (generate-env))]
         [valid-ctcs (filter (λ (c)
                                (or (equal? c ctc)
                                    (contract-stronger? c ctc)))
                             keys)])
    (if (> (length valid-ctcs) 0)
      (oneof (oneof (map (λ (key)
                            (hash-ref (generate-env) key))
                         valid-ctcs)))
      (make-generate-ctc-fail))))

; generate/indirect-env :: contract int -> (int -> value for contract)
; Attempts to make a generator that generates values for this contract
; by calling functions in the environment
(define (generate/indirect-env ctc fuel)
  (if (> fuel 0)
    (make-generate-ctc-fail)
    (make-generate-ctc-fail)))

; Given a contract and a value, attempts to verify that the contract
; matches the value given (generating arguments to functions if necessary)
; True if matching, false otherwise
(define (check-ctc-val ctc val fuel)
  ; If the contract is flat we can check immediately
  (if (flat-contract? ctc)
    ((flat-contract-predicate ctc) val)
    ; Our contract is not flat and not an ->, check for exercise field
    (let ([ex-c (contract-struct-exercise ctc)])
      (if (generate-ctc-fail? ex-c)
        (error "Could not find exerciser for contract: ~a\n" ex-c)
        (ex-c val fuel)))))

