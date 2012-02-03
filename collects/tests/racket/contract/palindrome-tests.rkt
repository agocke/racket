#lang racket/base

(require racket/contract)

(struct palindrome (str rev)
  #:property prop:equal+hash
  (list (λ (p1 p2 rec)
           (and (equal? (palindrome-str p1) (palindrome-str p2))
                (equal? (palindrome-rev p1) (palindrome-rev p2))))
        (λ (p rec) (equal-hash-code p))
        (λ (p rec) (equal-secondary-hash-code p))))

(define str-rev (compose list->string reverse string->list))

(define (palindrome->string palin)
  (string-append (palindrome-str palin) (palindrome-rev palin)))

(provide palindrome?)

(provide/contract (str-rev (string? . -> . string?))
                  (palindrome (string? string? . -> . palindrome?))
                  (palindrome->string (palindrome? . -> . string?)))
