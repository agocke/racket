#lang racket/base


(provide exercise-fail
         exercise-gen-fail
         exercise-missing
         (struct-out exn:fail:contract:exercise)
         (struct-out exn:fail:contract:exercise:gen-fail)
         (struct-out exn:fail:contract:exercise:ex-missing))

(struct exn:fail:contract:exercise exn:fail:contract (ctc))

(struct exn:fail:contract:exercise:gen-fail exn:fail:contract:exercise ())

(struct exn:fail:contract:exercise:ex-missing exn:fail:contract:exercise ())

(define (exercise-fail ctc 
                       reason
                       [exn-type exn:fail:contract:exercise])
  (let ([msg (format "Failed to exercise contract ~s: ~a" ctc reason)])
    (raise (exn-type msg
                     (current-continuation-marks)
                     ctc))))

(define (exercise-gen-fail ctc)
  (exercise-fail ctc "Generate failed" exn:fail:contract:exercise:gen-fail))

(define (exercise-missing ctc)
  (exercise-fail ctc "No exerciser" exn:fail:contract:exercise:ex-missing))
