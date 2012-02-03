#lang racket/base

(require "exercise-base.rkt"
         "generate.rkt"
         "guts.rkt"
         "prop.rkt")

(provide contract-exercise-modules
         contract-exercise-funs)

; contract-exercise-funs :: (list funcs) [(list func-names)] fuel -> .
;; The main worker function for exercising.
(define (contract-exercise-funs avail-funcs
                                func-names
                                #:fuel [fuel 5]
                                #:num-tests [num-tests 1]
                                #:print-gen [print-gen #f])
  ; track total tests, tests passed, tests failed, test failed due to generate
  (struct stats (total passed failed genf) #:mutable)
  (define run-stats (stats 0 0 0 0))
  (define (make-fail name)
    (exercise-exn name
                  (λ () 
                     (set-stats-total! run-stats
                                       (+ 1 (stats-total run-stats)))
                     (set-stats-failed! run-stats 
                                        (+ 1 (stats-failed run-stats))))))
  (define (make-gen name)
    (exercise-exn name
                  (λ () 
                     (set-stats-genf! run-stats
                                      (+ 1 (stats-genf run-stats))))))
  (define (print-results)
    (eprintf "Ran ~s tests, got ~s passes and ~s failures.\n"
             (stats-total run-stats)
             (stats-passed run-stats)
             (stats-failed run-stats))
    (unless (zero? (stats-genf run-stats))
      (eprintf "Could not generate ~s contract(s).\n" (stats-genf run-stats))))
  (let ([env (apply make-env-from-funs avail-funcs)])
    (parameterize ([generate-env env])
      (for ([func avail-funcs]
            [name func-names]
            #:when (has-contract? func))
        (let* ([ctc (value-contract func)]
               [exercise-fun (contract-struct-exercise ctc)])
          (with-handlers ([exn:fail:contract:exercise:gen-fail?
                            (make-gen name)]
                          [exn:fail? (make-fail name)])
            (begin (eprintf "testing ~a\n" name)
                   (exercise-fun func fuel #:print-gen print-gen)
                   (set-stats-passed! run-stats 
                                      (+ 1 (stats-passed run-stats)))
                   (set-stats-total! run-stats
                                     (+ 1 (stats-total run-stats))))))))
    (print-results)))

;; contract-exercise-modules :: module-path [integer?]
;; The module-level testing function. It is called on a module path
;; and individually exercises each available export with a contract
;; attached. Output is on the current-error-port.
(define (contract-exercise-modules module-paths 
                                   #:exclude [exclude null]
                                   #:fuel [fuel 5]
                                   #:num-tests [num-tests 1]
                                   #:print-gen [print-gen #f])
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
                            #:num-tests num-tests
                            #:print-gen print-gen)))


;; exercise-exn :: contract -> exception handler
(define (exercise-exn ctc [tracker #f])
  (λ (exn)
     (begin (when tracker (tracker))
            (eprintf "Got exception while processing function ~a\n" ctc)
            (if (exn:fail:contract:exercise:gen-fail? exn)
                (eprintf "~a\n" (exn-message exn))
                ((error-display-handler) (exn-message exn) exn)))))

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
