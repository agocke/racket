#lang racket/base

(require racket/contract
         rackunit
         rackunit/text-ui
         net/url)

(require "private-generate-tests.rkt"
         "private-arrow-tests.rkt"
         "palindrome-tests.rkt")

(define (test-contract-generation ctc 
                                  [monkey-with values] 
                                  #:size [size 10])
  (define example-vals (contract-random-generate ctc size))
  (monkey-with (contract ctc example-vals 'pos 'neg)))

(define pred-tests
  (test-suite
   "Predicate contract"
   (check-not-exn (λ () (test-contract-generation integer?)))
   (check-not-exn (λ () (test-contract-generation exact-nonnegative-integer?)))
   (check-not-exn (λ () (test-contract-generation boolean?)))
   (check-not-exn (λ () (test-contract-generation char?)))
   (check-not-exn (λ () (test-contract-generation byte?)))
   (check-not-exn (λ () (test-contract-generation bytes?)))
   (check-not-exn (λ () (test-contract-generation string?)))
   (check-not-exn (λ () (test-contract-generation contract?)))
   (check-not-exn (λ () (test-contract-generation symbol?)))
  ))

(define flat-ctc-tests
  (test-suite
    "Built-in flat contracts"
    (check-not-exn (λ () (test-contract-generation (between/c 1 100))))
    (check-not-exn (λ () (test-contract-generation (listof integer?))))
    (check-not-exn (λ () (test-contract-generation (>=/c 0))))
    (check-not-exn (λ () (test-contract-generation (<=/c 0))))
    (check-not-exn (λ () (test-contract-generation (>/c 0))))
    (check-not-exn (λ () (test-contract-generation (</c 0))))
    (check-not-exn (λ () (test-contract-generation (or/c boolean? boolean?))))
    (check-equal? (test-contract-generation #f) #f)
    (check-equal? (test-contract-generation 10) 10)
    ))

(define func-tests
  (test-suite
    "Function contracts"
    (check-exn exn:fail? (λ ()
                            ((test-contract-generation (-> char?
                                                           integer?)) 0)))
    (check-not-exn (λ () ((test-contract-generation (-> integer?
                                                        integer?)) 1)))
    (λ () ((test-contract-generation 
                             (-> (-> integer? 
                                     integer?)
                                 boolean?)) 
                            +))
    ))

(define exercise-tests
  (test-suite 
    "Random contract exercise tests"
    (check-not-exn
      (λ () 
         (contract-exercise-modules
           '("collects/tests/racket/contract/palindrome-tests.rkt"))))))

(define ctc-gen-tests
  (test-suite
    "Random contract generation tests"
    pred-tests
    flat-ctc-tests
    func-tests))

(define all-module-tests
  (test-suite
    "All random contract tests"
    ctc-gen-tests
    exercise-tests
    private/generate-tests
    private/arrow-tests))

(run-tests all-module-tests)
