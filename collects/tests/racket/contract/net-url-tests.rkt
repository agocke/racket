#lang racket/base

(require racket/contract
         racket/set
         net/url)

(contract-add-generate url?
  (λ (fuel) (string->url "http://google.com")))

(contract-exercise-modules '(net/url)
                           #:exclude '(string->url 
                                        path->url 
                                        url 
                                        netscape/string->url
                                        combine-url/relative
                                        path/param)
                           #:tests 30)
