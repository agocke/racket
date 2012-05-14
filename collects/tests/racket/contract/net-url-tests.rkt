#lang racket/base

(require racket/contract
         net/url
         slideshow
         plot)

(contract-add-generate url?
  (λ (fuel) (string->url "http://google.com")))

(define (trace all-traces)
  (eprintf "traces ~s\n"(length all-traces)))

(contract-exercise-modules '(net/url)
                           #:exclude '(string->url 
                                        path->url 
                                        url 
                                        netscape/string->url
                                        combine-url/relative
                                        path/param)
                           #:trace trace)

