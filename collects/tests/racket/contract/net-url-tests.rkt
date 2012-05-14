#lang racket/base

(require racket/contract
         net/url
         slideshow
         plot)

(contract-add-generate url?
  (λ (fuel) (string->url "http://google.com")))

(define all-traces null)

(define (trace exercise-trace generate/direct-trace generate/env-trace
               generate/indirect-trace)
  (set! all-traces (append all-traces
                           (list exercise-trace
                                 generate/direct-trace
                                 generate/env-trace
                                 generate/indirect-trace))))

(contract-exercise-modules '(net/url)
                           #:exclude '(string->url 
                                        path->url 
                                        url 
                                        netscape/string->url
                                        combine-url/relative
                                        path/param)
                           #:trace trace)

(length all-traces)

