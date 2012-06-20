#lang racket/base

(require racket/contract
         racket/set
         rackunit
         rackunit/text-ui
         "../../../racket/contract/private/prop.rkt"
         "palindrome-tests.rkt")

(provide private/arrow-tests)

(define fun-env (make-env-from-funs str-rev
                                    palindrome
                                    palindrome->string))

(define-values (str-rev-ctc 
                palindrome-ctc
                palindrome->string-ctc)
  (values (value-contract str-rev)
          (value-contract palindrome)
          (value-contract palindrome->string)))

(define (check-can-generate ctc expected-cts)
  (let ([actual (map (compose symbol->string contract-struct-name) 
                     ((contract-struct-can-generate ctc) 'exercise))]
        [expected (map symbol->string expected-cts)])
    (check-equal? (sort actual string<?) (sort expected string<?))))

(define ->-can-generate-tests
  (test-suite
   "->-can-generate tests"
   (check-can-generate (integer? . -> . integer?)
                       '(integer?))
   (check-can-generate (value-contract str-rev)
                       '(string?))
   (check-can-generate (value-contract palindrome)
                       '(palindrome?))
   (check-can-generate (value-contract palindrome->string)
                       '(string?))
   (check-can-generate ((integer? . -> . char?)
                        . -> .
                        string?)
                       '(integer? string?))
   (check-can-generate ((integer? . -> . (boolean? . -> . char?))
                        . -> .
                        string?)
                       '(boolean? integer? string?))
   (check-can-generate ((integer? . -> . char?)
                        . -> .
                        integer?)
                       '(integer?))
   ))

(define ->-exercise-tests
  (test-suite
    "->-exercise-tests"
    (parameterize ([generate-env fun-env])
      (check-not-exn (λ ()
                        ((contract-struct-exercise str-rev-ctc)
                         str-rev
                         5
                         #f))))))

(define private/arrow-tests
  (test-suite
    "contract/private/arrow.rkt unit tests"
    ->-can-generate-tests
    ->-exercise-tests))

(run-tests private/arrow-tests)
