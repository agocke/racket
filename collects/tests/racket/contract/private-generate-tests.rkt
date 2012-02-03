#lang racket/base

(require racket/contract
         racket/list
         racket/sequence
         rackunit
         rackunit/text-ui
         "../../../racket/contract/private/generate-base.rkt"
         "../../../racket/contract/private/generate.rkt"
         "rand-module-tests.rkt")

(provide private/generate-tests)

(define-values (str-rev-ctc 
                palindrome-ctc
                palindrome->string-ctc)
  (values (value-contract str-rev)
          (value-contract palindrome)
          (value-contract palindrome->string)))

(define fun-env (make-env-from-funs str-rev
                                    palindrome
                                    palindrome->string))

(define filter-vals-tests
  (test-suite
   "filter-vals tests"
   (let ([pred (valid-gen (flat-contract string?))]
         [matching (list (cons str-rev-ctc str-rev)
                         (cons palindrome->string-ctc
                               palindrome->string))])
     (check-true (for/and ([pair (filter-vals pred fun-env)])
                          (ormap (λ (p) (equal? p pair)) matching))))
   (let ([pred (valid-gen (flat-contract palindrome?))])
     (check-equal? (sequence->list (filter-vals pred fun-env))
                   (list (cons palindrome-ctc palindrome))))))

(define find-val-fun-tests
  (test-suite
   "find-val-fun tests"
   (let ([a-val (find-val-fun (flat-contract string?) fun-env)]
         [palin (find-val-fun (flat-contract palindrome?) fun-env)])
     (check-false (void? a-val))
     (check-equal? (palindrome "ab" "ba")
                   ((cdr palin) "ab" "ba")))))

(define generate/indirect-env-tests
  (test-suite
   "generate/indirect-env tests"
   (parameterize ([generate-env (make-env-from-funs palindrome)])
     (check-pred palindrome? (generate/indirect-env (flat-contract palindrome?) 10)))
   (parameterize ([generate-env fun-env])
     (check-pred palindrome? (generate/indirect-env (flat-contract palindrome?) 10))
     (check-pred string? (generate/indirect-env (flat-contract string?) 10)))))

(define valid-gen-tests
  (test-suite
   "valid-gen tests"
   (let ([pred (valid-gen (flat-contract string?))])
     (check-true (pred str-rev-ctc (list str-rev)))
     (check-false (pred palindrome-ctc (list palindrome)))
     (check-true (pred palindrome->string-ctc (list palindrome->string))))
   (let ([pred (valid-gen (flat-contract palindrome?))])
     (check-false (pred str-rev-ctc (list str-rev)))
     (check-true (pred palindrome-ctc (list palindrome)))
     (check-false (pred palindrome->string-ctc (list palindrome->string))))))

(define private/generate-tests
  (test-suite
    "contract/private/generate.rkt unit tests"
    filter-vals-tests
    find-val-fun-tests
    generate/indirect-env-tests
    valid-gen-tests))

(define (list-set-equal? l1 l2)
  (for/and ([v l1])
    (let ([ev (λ (v2) (equal? v v2))])
      (= (count ev l1) (count ev l2)))))

(run-tests private/generate-tests)
