#lang racket/base

(require (for-syntax scheme/base))

(provide rand
         rand-seed
         rand-choice)


;; random generator

(define my-generator (make-pseudo-random-generator))
(define (rand [x #f]) 
  (if x
      (random x my-generator)
      (random my-generator)))


(define (rand-seed x)
  (parameterize ([current-pseudo-random-generator my-generator])
    (random-seed x)))

(rand-seed 0)

(define-syntax (rand-choice stx)
  (syntax-case stx ()
    [(_ (a case1 case2 ...) ...)
     (begin
       (let ([ns (let loop ([sum 0]
                            [as (syntax->list #'(a ...))])
                   (cond
                     [(null? as) (raise-syntax-error #f "expected at least one case" stx)]
                     [(null? (cdr as))
                      (syntax-case (car as) (else)
                        [else (list (- 1 sum))]
                        [_ (raise-syntax-error #f "expected last option to be `else'" stx (car as))])]
                     [else
                      (let ([n (syntax-e (car as))])
                        (unless (and (real? n)
                                     (exact? n)
                                     (positive? n)
                                     (< n 1))
                          (raise-syntax-error #f "expected each option to be a real exact number in the interval (0,1)" stx (car as)))
                        (unless (< (+ n sum) 1)
                          (raise-syntax-error #f "expected the sum of the options to be less than 1" stx #f (syntax->list #'(a ...))))
                        (cons n (loop (+ sum n)
                                      (cdr as))))]))])
         (let* ([dens (map denominator ns)]
                [choices (map (λ (x) (* (numerator x) (apply * (remove
                                                                (denominator x) dens))))
                              ns)])
           #`(rand-choice/proc '(#,@choices)
                               #,(apply * dens)
                               (list (λ () case1 case2 ...) ...)))))]))

(define (rand-choice/proc choices prod thunks)
  (let ([choice (rand prod)])
    (let loop ([n choice]
               [nums choices]
               [thunks thunks])
      (cond
        [(null? nums) (error 'rand-chance "internal error!")]
        [else
         (cond
           [(< n (car nums))
            ((car thunks))]
           [else
            (loop (- n (car nums))
                  (cdr nums)
                  (cdr thunks))])]))))
