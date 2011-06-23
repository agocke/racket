#lang racket/base

;; A stripped down version of scheme/contract for use in
;; the PLT code base where appropriate.

(require "private/arrow.rkt"
         "private/arr-i.rkt"
         "private/base.rkt"
         "private/box.rkt"
         "private/hash.rkt"
         "private/vector.rkt"
         "private/struct.rkt"
         "private/misc.rkt"
         "private/provide.rkt"
         "private/guts.rkt"
         "private/opters.rkt"
         "private/opt.rkt")

(provide
 (except-out (all-from-out "private/arrow.rkt")
             making-a-method
             procedure-accepts-and-more?
             check-procedure
             check-procedure/more

             contracted-function?
             contracted-function-proc
             contracted-function-ctc
             make-contracted-function)
 (all-from-out "private/arr-i.rkt"
               "private/box.rkt"
               "private/hash.rkt"
               "private/vector.rkt"
               "private/struct.rkt")
 (except-out (all-from-out "private/base.rkt")
             current-contract-region)
 (except-out (all-from-out "private/misc.rkt")
             check-between/c
             check-unary-between/c)
 (all-from-out "private/provide.rkt")

 ;; from private/opt.rkt:
 opt/c define-opt/c

 ;; from private/guts.rkt
 has-contract?
 value-contract
)
