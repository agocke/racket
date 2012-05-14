#lang racket/base

(require "exercise-base.rkt"
         "generate.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "prop.rkt")

(provide contract-exercise-modules
         contract-exercise-funs
         contract-random-exercise
         exercise-fail
         exercise-gen-fail)

(define exercise-trace (make-parameter #f))

(define (contract-random-exercise 
          ctc
          val
          fuel
          print-gen
          #:tests [num-tests 1])
  (let ([ex-trace (exercise-trace)])
    (when ex-trace
      (let ([name (contract-struct-name ctc)])
        (hash-update! ex-trace
                      name
                      (λ (i) (+ i 1))
                      0))))
  (for ([i (in-range num-tests)])
       ((contract-struct-exercise ctc) val fuel print-gen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise statistics and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (increment-keys run-stats keys)
  (for ([k keys])
       (hash-update! run-stats k (λ (i) (+ i 1)))))

;; exercise-exn :: string? -> exception handler
(define (((exercise-exn-handler fun-name run-stats) increments) exn)
  (begin (increment-keys run-stats increments)
         (eprintf "Got exception while processing function ~a\n" fun-name)
         (if (or (exn:fail:contract:exercise:gen-fail? exn)
                 (exn:fail:contract:exercise:ex-missing? exn))
           (displayln (exn-message exn)
                      (current-error-port))
           ((error-display-handler) (exn-message exn) exn))))


(define (do-exercise-prolog name)
  (eprintf "testing ~a\n" name))

(define (do-top-level-exercise ctc func fuel print-gen num-tests trace)
  (define (run)
    (contract-random-exercise ctc func fuel print-gen #:tests num-tests))
  (if trace
    (parameterize ([exercise-trace (make-hash)]
                   [generate/direct-trace (make-hash)]
                   [generate/env-trace (make-hash)]
                   [generate/indirect-trace (make-hash)])
        (run)
        (trace (exercise-trace)
               (generate/direct-trace)
               (generate/env-trace)
               (generate/indirect-trace)))
    (run)))

(define (do-exercise-epilog run-stats)
  (increment-keys run-stats '(passed total)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; contract-exercise-funs :: (list funcs) [(list func-names)] fuel -> .
;; The main worker function for exercising.
(define (contract-exercise-funs avail-funcs
                                func-names
                                #:fuel [fuel 5]
                                #:tests [num-tests 1]
                                #:print-gen [print-gen #f]
                                #:trace [trace #f])
  (define run-stats 
    (make-hash (list '(total . 0)
                     '(passed . 0)
                     '(failed . 0)
                     '(genf . 0)
                     '(exm . 0))))
  (define (print-results)
    (define (get k) (hash-ref run-stats k))
    (eprintf "Ran ~s tests, got ~s passes and ~s failures.\n"
             (get 'total)
             (get 'passed)
             (get 'failed))
    (unless (zero? (get 'genf))
      (eprintf "Could not generate ~a contract(s).\n" (get 'genf)))
    (unless (zero? (get 'exm))
      (eprintf "Missing exerciser for ~a contract(s).\n" (get 'exm))))
  (let ([env (apply make-env-from-funs avail-funcs)])
    (parameterize ([generate-env env])
      (for ([func avail-funcs]
            [name func-names]
            #:when (has-contract? func))
        (let* ([ctc (value-contract func)]
               [handler (exercise-exn-handler name run-stats)])
          (with-handlers ([exn:fail:contract:exercise:gen-fail?
                            (handler '(genf))]
                          [exn:fail:contract:exercise:ex-missing?
                            (handler '(exm))]
                          [exn:fail?
                            (handler '(total failed))])
            (do-exercise-prolog name)
            (do-top-level-exercise ctc func fuel 
                                   print-gen num-tests trace)
            (do-exercise-epilog run-stats))))))
  (print-results))

;; contract-exercise-modules :: module-path [integer?]
;; The module-level testing function. It is called on a module path
;; and individually exercises each available export with a contract
;; attached. Output is on the current-error-port.
(define (contract-exercise-modules module-paths 
                                   #:exclude [exclude null]
                                   #:fuel [fuel 5]
                                   #:tests [num-tests 1]
                                   #:print-gen [print-gen #f]
                                   #:trace [trace #f])
  (define (get-funs+names mod)
    (let* ([export-names (get-exports mod)]
           [minus-excluded (remove* exclude export-names)]
           [exports (for/list ([provided minus-excluded])
                      (with-handlers ([exn:fail? (λ (exn) #f)])
                        (dynamic-require mod provided)))])
      (for/lists (funs names)
                 ([export exports]
                  [name minus-excluded]
                  #:when (procedure? export))
        (values export name))))
  (let-values ([(all-funs all-names)
                (for/lists (funs names)
                           ([mod module-paths])
                  (get-funs+names mod))])
    (contract-exercise-funs (apply append all-funs)
                            (apply append all-names)
                            #:fuel fuel
                            #:tests num-tests
                            #:print-gen print-gen
                            #:trace trace)))


;; get-exports : module-path -> (or/c #f (listof symbol))
(define (get-exports quoted-module-path)
 (define src-path
   (resolved-module-path-name
    ((current-module-name-resolver) quoted-module-path #f #f #t)))
 (define zo-path
   (let-values ([(base name dir?) (split-path src-path)])
     (build-path base
                 "compiled"
                 (bytes->path
                  (bytes-append
                   (regexp-replace #rx#"([.])([^.]*)$" (path->bytes name) 
                                   #"_\\2")
                   #".zo")))))
 (define module-code
   (with-handlers ((exn:fail:read? (λ (x) #f)))
     (parameterize ([read-accept-compiled #t])
       (call-with-input-file zo-path read))))
 (cond
   [(compiled-module-expression? module-code)
    (define-values (vars-table syn-table)
      (module-compiled-exports
       module-code))
    (define exports (assoc 0 syn-table))
    (and exports
         (map car (cdr exports)))]
   [else #f]))
