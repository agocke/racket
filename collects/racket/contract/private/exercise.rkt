#lang racket/base

(require "exercise-base.rkt"
         "exercise-main.rkt"
         "generate.rkt"
         "generate-base.rkt"
         "guts.rkt"
         "prop.rkt"
         "rand.rkt")

(provide contract-exercise-modules
         contract-exercise-funs
         contract-random-exercise
         exercise-fail
         exercise-gen-fail
         
         (struct-out single-exercise-trace))

(struct single-exercise-trace
        (name
          ctc-name
          exercise-trace
          generate/direct-trace
          generate/env-trace
          generate/indirect-trace))

; contract-exercise-funs :: (list funcs) [(list func-names)] fuel -> .
;; The main worker function for exercising.
(define (contract-exercise-funs vals+names
                                #:fuel [fuel 5]
                                #:tests [num-tests 1]
                                #:print-gen [print-gen #f]
                                #:trace [trace #f]
                                #:logging [logging #f])
  (eprintf "vals+names ~s\n" vals+names)
  ; Hash to hold all the run statistics
  (define run-stats 
    (make-hash (list '(total . 0)
                     '(passed . 0)
                     '(failed . 0)
                     '(genf . 0)
                     '(exm . 0))))

  ; Holds trace information if trace is #t
  (define traces null)

  ; Print the run statistics
  (define (print-results run-stats)
    (define (get k) (hash-ref run-stats k))
    (eprintf "Ran ~s tests, got ~s passes and ~s failures.\n"
             (get 'total)
             (get 'passed)
             (get 'failed))
    (unless (zero? (get 'genf))
      (eprintf "Could not generate ~a contract(s).\n" (get 'genf)))
    (unless (zero? (get 'exm))
      (eprintf "Missing exerciser for ~a contract(s).\n" (get 'exm))))

  ; Current environment
  (define env 
    (let ([vals (map car vals+names)])
      (bulk-env-add (map value-contract vals)
                    vals)))

  ; Save the incoming ports for restoration afterwards
  (define save-current-output #f)
  (define save-current-input #f)

  (define (do-prolog func-name ctc-name)
    (rand-seed 0)
    (eprintf "testing ~a ~a\n" func-name ctc-name))

  (define (run-exercise run ctc ctc-name func-name)
    (if trace
        (parameterize ([exercise-trace (make-hash)]
                       [generate/direct-trace (make-hash)]
                       [generate/env-trace (make-hash)]
                       [generate/indirect-trace (make-hash)])
          (run)
          (set! traces (cons (single-exercise-trace
                               (symbol->string func-name)
                               (format "~a" ctc-name)
                               (exercise-trace)
                               (generate/direct-trace)
                               (generate/env-trace)
                               (generate/indirect-trace))
                             traces)))
        (run)))

  (define (do-epilog run-stats)
    (increment-keys run-stats '(passed total)))

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

  ; Setup before the exercises
  (set! save-current-output (current-output-port))
  (current-output-port (generate/direct output-port? 0))
  (set! save-current-input (current-input-port))
  (current-input-port (generate/direct input-port? 0))

  ; Do the exercises
  (parameterize ([generate-env env]
                 [exercise-logging logging])
    (for ([val (map car vals+names)]
          [val-name (map cdr vals+names)]
          #:when (has-contract? val))
      (let* ([ctc (value-contract val)]
             [ctc-name (contract-struct-name ctc)]
             [handler (exercise-exn-handler val-name run-stats)])
        (with-handlers ([exn:fail:contract:exercise:gen-fail?
                          (handler '(genf))]
                        [exn:fail:contract:exercise:ex-missing?
                          (handler '(exm))]
                        [exn:fail?
                          (handler '(total failed))])
          (do-prolog val-name ctc-name)
          (run-exercise (λ ()
                           (contract-random-exercise ctc
                                                     val 
                                                     fuel
                                                     print-gen
                                                     #:tests num-tests))
                        ctc
                        ctc-name
                        val-name)
          (do-epilog run-stats)))))

  ; Done exercising: cleanup and print the results (and traces, if enabled)
  (current-output-port save-current-output)
  (current-input-port save-current-input)
  (print-results run-stats)
  (when trace (trace traces)))

;; contract-exercise-modules :: module-path [integer?]
;; The module-level testing function. It is called on a module path
;; and individually exercises each available export with a contract
;; attached. Output is on the current-error-port.
(define (contract-exercise-modules module-paths 
                                   #:exclude [exclude null]
                                   #:fuel [fuel 5]
                                   #:tests [num-tests 1]
                                   #:print-gen [print-gen #f]
                                   #:trace [trace #f]
                                   #:logging [logging #f])
  (define (get-vals+names mod)
    (let* ([export-names (get-exports mod)]
           [minus-excluded (if export-names
                               (remove* exclude export-names)
                               null)]
           [exports (for/list ([provided minus-excluded])
                      (with-handlers ([exn:fail? (λ (exn) #f)])
                        (dynamic-require mod provided)))])
      (for/lists (vals names)
                 ([export exports]
                  [name minus-excluded])
        (values export name))))
  (let-values ([(all-vals all-names)
                (for/lists (vals names)
                           ([mod module-paths])
                  (get-vals+names mod))])
    (contract-exercise-funs (map cons
                                 (apply append all-vals)
                                 (apply append all-names))
                            #:fuel fuel
                            #:tests num-tests
                            #:print-gen print-gen
                            #:trace trace
                            #:logging logging)))


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
         (sort (map car (cdr exports))
               (λ (a b) (string<? (symbol->string a)
                                  (symbol->string b)))))]
   [else #f]))
