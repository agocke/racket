#lang racket/base

(provide exercise-fail
         exercise-gen-fail
         exercise-missing
         (struct-out exn:fail:contract:exercise)
         (struct-out exn:fail:contract:exercise:gen-fail)
         (struct-out exn:fail:contract:exercise:ex-missing)
         
         exercise-trace
         (struct-out single-exercise-trace)
         add-trace
         exercise-output-port)

(struct exn:fail:contract:exercise exn:fail:contract (ctc-name))

(struct exn:fail:contract:exercise:gen-fail exn:fail:contract:exercise ())

(struct exn:fail:contract:exercise:ex-missing exn:fail:contract:exercise ())

(define (exercise-fail ctc-name 
                       reason
                       [exn-type exn:fail:contract:exercise])
  (let ([msg (format "Exercise failed for ~s: ~a" ctc-name reason)])
    (raise (exn-type msg
                     (current-continuation-marks)
                     ctc-name))))

(define (exercise-gen-fail ctc-name)
  (exercise-fail ctc-name "Generate failed" exn:fail:contract:exercise:gen-fail))

(define (exercise-missing ctc-name)
  (exercise-fail ctc-name "No exerciser" exn:fail:contract:exercise:ex-missing))

; Exercise tracing
(define exercise-trace (make-parameter #f))

; Struct holding the traces for a single exercise
(struct single-exercise-trace (name ctc-name traces))

; Add trace if tracing is enabled
(define (add-trace ctc-name location)
  (let ([ex-trace (exercise-trace)])
    (when ex-trace
      (set-box! ex-trace (cons (cons ctc-name
                                 location)
                               (unbox ex-trace))))))

(define exercise-output-port (make-parameter #f))
