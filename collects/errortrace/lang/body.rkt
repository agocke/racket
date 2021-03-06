#lang racket/base
(require (for-syntax racket/base
                     syntax/strip-context
                     "../errortrace-lib.rkt"))

(provide (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ lang . body)
     (let ([e (annotate-top
               (syntax-local-introduce
                (local-expand #`(module . #,(strip-context #`(n lang . body)))
                              'top-level
                              null))
               0)])
       (syntax-case e ()
         [(mod nm lang (mb . body)) 
          #`(#%plain-module-begin 
             (require (only-in lang) errortrace/errortrace-key)
             . body)]))]))
