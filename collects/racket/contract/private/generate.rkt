#lang racket/base

(require "rand.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "arrow.rkt"
         racket/list)

(provide
 use-env
 env-item
 generate)

; env
(define gen-env (make-thread-cell (make-hash)))

;; hash tables
;(define freq-hash (make-hash))
;(define gen-hash (make-hash))

;; thread-cell
;(define arg-names-count (make-thread-cell 0))

;; generate integer? 
(add-generate integer?
           (λ (n-tests size env)
             (rand-choice
              [1/10 0]
              [1/10 1]
              [1/10 -1]
              [1/10 2147483647]
              [1/10 -2147483648]
              [3/10 (- 100 (rand 200))]
              [else (- 1000000000 (rand 2000000000))])))

(add-generate exact-nonnegative-integer?
               (λ (n-tests size env)
                 (abs ((find-generate integer?) n-tests size env))))


(add-generate positive?
           (λ (n-tests size env)
             (rand-choice
              [1/10 1]
              [1/10 1/3]
              [1/10 0.12]
              [1/10 2147483647]
              [else 4])))

(add-generate boolean?
           (λ (n-tests size env)
             (define (boolean?-static n-tests size env)
               (rand-choice
                [1/2 #t]
                [else #f]))
             
             (rand-choice
              [2/3 (boolean?-static n-tests size env)]
              [else (let-values ([(res v) (use-env n-tests size env boolean?)])
                      (if res
                          v
                          (boolean?-static n-tests size env)))])))




;; TODO: add new field to each contract. Move the code to guts
;; The set of functions beelow needs to be moved into guts.rkt
;; A new property for every contract has to be added with a name of
;; can-generate-given (or some better name). The interface looks like this:
;; can-generate-given: val -> (list-of ((list-of in-ctc), out-ctc, lambda-from-in-ctc-to-out-ctc))
;; in other words given a val with a contract we get the contract of this value. Than we ;; get the implementation of can-generate-given from the contract property and using it 
;; we generate a list of tuples. Each tuple shows one possible use of this val in generating 
;; out-ctc give a number of in-ctc. The last element in the tuple is something that can
;; be used to generate the right output given the right input.



(define (gen-opts have-val want-ctc have-ctc n-tests size env)
  (append (if (contract-stronger? have-ctc want-ctc)
              (list have-val)
              '())
          (if (base->? have-ctc)
              (let* ([gens (map contract-struct-generate
                                (base->-doms/c have-ctc))])
                (if (member #f gens)
                    '()
                    (let useful-results ([result-ctcs (base->-rngs/c have-ctc)]
                                         [i 0])
                      (if (empty? result-ctcs)
                          '()
                          (let* ([args (map (λ (g)
                                              ; what should n-tests and size be
                                              (g 0 0 env))
                                            gens)])
                            (append (gen-opts (λ ()
                                                (call-with-values (λ ()
                                                                    (apply have-val args))
                                                                  (λ args
                                                                    (list-ref args i)))) 
                                              want-ctc 
                                              (first result-ctcs)
                                              n-tests
                                              size
                                              env)
                                    (useful-results (rest result-ctcs) (+ i 1))))))))
                '())))

(define (use-env n-tests size env ctc
                 #:test [is-test #f])
  (let ([options (flatten (map (λ (e-i)
                                 ;; contact-stronger? stronger weaker -> #
                                 (gen-opts (env-item-name e-i)
                                           ctc
                                           (env-item-ctc e-i)
                                           n-tests
                                           size
                                           env))
                               env))])
    
    (if (not (null? options))
        (values #t (if is-test
                       options
                       ((list-ref options (rand (length options))))))
        (values #f #f))))


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

; generate : contract -> ??
(define (generate ctc)
 (let ([options (permute '(generate/direct
                            generate/direct-env
                            generate/indirect-env))])
   ; choose randomly
   (or ((first options))
       ((second options))
       ((third options))
       (error "Unable to construct any generator for contract: ~a"
              ctc))))

; generate/direct :: contract -> (int int -> value for contract)
; Attempts to make a generator that generates values for this contract
; directly. Returns #f if making a generator fails.
(define (generate/direct ctc)
  (let ([g (contract-struct-generate ctc)])
    (or g #f)))

(define (generate/direct-env ctc)
  (let* ([keys (hash-keys (thread-cell-ref gen-env))]
        [valid-ctcs (filter (λ (c)
                               (or (equal? c ctc)
                                   (contract-stronger? c ctc)))
                            keys)])
    (if (> (length valid-ctcs) 0)
      (oneof (map (λ (key)
                     (hash-ref (thread-cell-ref key)))
                  valid-ctcs))
      #f)))

(define (generate/indirect-env ctc)
  #f)
