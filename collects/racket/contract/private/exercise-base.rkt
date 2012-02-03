#lang racket/base

(provide exercise-fail
         exercise-gen-fail
         (struct-out exn:fail:contract:exercise)
         (struct-out exn:fail:contract:exercise:gen-fail))

(struct exn:fail:contract:exercise exn:fail:contract (ctc))

(struct exn:fail:contract:exercise:gen-fail exn:fail:contract:exercise ())

(define (exercise-fail ctc reason)
  (let ([msg (format "Failed to exercise contract: ~s ~a" ctc reason)])
    (raise (exn:fail:contract:exercise msg 
                                       (current-continuation-marks)
                                       ctc))))

(define (exercise-gen-fail ctc)
  (let ([msg (format "Generate failed on: ~s" ctc)])
    (raise (exn:fail:contract:exercise:gen-fail msg
                                                (current-continuation-marks)
                                                ctc))))
