#lang racket/base

(require racket/list
         racket/sequence
         "rand.rkt")

(provide gen-fail-map
         generate-env
         find-generate
         get-env-contracts
         contract-add-generate
         
         (struct-out generate-ctc-fail))

;; generate failure type
(struct generate-ctc-fail (ctc))
                 
;; for/generate-fail
;; Like for/list, but if it hits a generate-ctc-fail, returns generate-ctc-fail
(define (gen-fail-map fun lst)
  (let ([nlst (sequence->list (stop-after (sequence-map fun (in-list lst))
                                          generate-ctc-fail?))])
    (cond [(null? nlst) nlst]
          [(generate-ctc-fail? (last nlst)) (last nlst)]
          [else nlst])))

; env parameter
(define generate-env (make-parameter #f))

(define make-pairs 
  (λ args
     (if (null? args)
         null
         (cons (cons (car args) (cadr args))
               (apply make-pairs (cddr args))))))

(define gen-hash 
  (make-hash
   (make-pairs
    number?
    (λ (fuel)
      (let ([real-part ((find-generate real?) fuel)])
        (rand-choice
         [1/10 (make-rectangular real-part
                                 ((find-generate real?) fuel))]
         [else real-part])))

    complex?
    (λ (fuel)
       ((find-generate number?) fuel))

    real?
    (λ (fuel)
      (rand-choice
       [1/20 +inf.0]
       [1/20 +inf.f]
       [1/20 -inf.0]
       [1/20 -inf.f]
       [1/20 +nan.0]
       [1/20 +nan.f]
       [else ((find-generate rational?) fuel)]))

    rational?
    (λ (fuel)
      (let ([pos ((find-generate positive?) fuel)])
        (rand-choice
         [1/2 pos]
         [else (- pos)])))

    integer?
    (λ (fuel)
      (rand-choice
       [1/10 0]
       [1/10 1]
       [1/10 -1]
       [1/10 2147483647]
       [1/10 -2147483648]
       [3/10 (rand-range -100 200)]
       [else (rand-range -1000000000 2000000000)]))

    exact-nonnegative-integer?
    (λ (fuel)
      (abs ((find-generate integer?) fuel)))

    positive?
    (λ (fuel)
      (rand-choice
       [1/10 1]
       [1/10 1/3]
       [1/10 0.12]
       [1/10 2147483647]
       [else ((find-generate integer?) fuel)]))

    boolean?
    (λ (fuel)
      (rand-choice
       [1/2 #t]
       [else #f]))
    
    char?
    (λ (fuel)
       (integer->char (rand-choice
                  [19/20 (rand-range 32 126)]
                  [else (oneof (list (rand-range 0 55295)
                                     (rand-range 57344 1114111)))])))

    string?
    (λ (fuel)
      (let* ([len (rand-choice [1/10 0]
                               [1/10 1]
                               [else (rand-range 2 260)])]
             [strl (build-list len 
                               (λ (x) ((find-generate char?) fuel)))])
        (apply string strl)))
    
    byte?
    (λ (fuel)
      (rand 256))
   
    bytes?
    (λ (fuel)
      (let* ([len (rand-choice
                   [1/10 0]
                   [1/10 1]
                   [else (+ 2 (rand 260))])]
             [bstr (build-list len
                               (λ (x)
                                 (rand 256)))])
        (apply bytes bstr)))
    
    symbol?
    (λ (fuel)
       (string->symbol ((find-generate string?) fuel))))))

;; contract-add-generate :: predicate generator -> .
(define (contract-add-generate predicate generator)
  (hash-set! gen-hash predicate generator))

;; thread-cell
(define arg-names-count (make-thread-cell 0))

;; given a predicate returns a generate for this predicate or generate-ctc-fail
(define (find-generate func [name "internal"])
  (hash-ref gen-hash func (λ ()
                             (generate-ctc-fail func))))

;; all the contracts available in the current environment
(define (get-env-contracts)
  (let* ([env (generate-env)]
         [gen-ctcs (and env (hash-keys (generate-env)))]
         [pred-ctcs (hash-keys gen-hash)])
    (if gen-ctcs
        (append gen-ctcs pred-ctcs)
        pred-ctcs)))

(define (get-arg-names-space space-needed)
  (let ([rv (thread-cell-ref arg-names-count)])
    (thread-cell-set! arg-names-count (+ rv space-needed))
    rv))

(define (gen-arg-names st-num size)
  (cond
    [(<= size 0) (list)]
    [else (cons (string->symbol (string-append "x-" (number->string st-num)))
                (gen-arg-names (+ st-num 1) (- size 1)))]))


