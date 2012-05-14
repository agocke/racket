#lang racket/base

(require racket/contract
         racket/set
         net/url
         slideshow
         plot)

(contract-add-generate url?
  (λ (fuel) (string->url "http://google.com")))

; traces : (listof hash)
(define (merge-traces traces)
  (let ([all-ctcs (remove-duplicates (append-map hash-keys traces))])
    (for/list ([ctc all-ctcs])
      (let ([v vector-immutable])
        (list (v 'exercise (hash-ref (first traces) ctc 0))
              (v 'generate/direct (hash-ref (second traces) ctc 0))
              (v 'generate/env (hash-ref (third traces) ctc 0))
              (v 'generate/indirect (hash-ref (fourth traces) ctc 0)))))))

(define (plot-traces traces)
  (slide (plot-pict (for/list ([trace (merge-traces (cdr traces))])
                      (discrete-histogram trace)))))

(define (trace all-traces)
  (eprintf "trace length ~s\n" (length all-traces))
  (for-each plot-traces
            all-traces))

(contract-exercise-modules '(net/url)
                           #:exclude '(string->url 
                                        path->url 
                                        url 
                                        netscape/string->url
                                        combine-url/relative
                                        path/param)
                           #:trace trace)
