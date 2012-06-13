#lang racket/base

(require (for-syntax racket/base)
         racket/promise
         racket/sequence
         "prop.rkt"
         "blame.rkt"
         "guts.rkt"
         "rand.rkt"
         "generate-base.rkt"
         "generate.rkt"
         "exercise.rkt")

(provide flat-rec-contract
         flat-murec-contract
         or/c 
         and/c
         not/c
         =/c >=/c <=/c </c >/c between/c
         integer-in
         real-in
         natural-number/c
         string-len/c
         false/c
         printable/c
         symbols one-of/c
         listof non-empty-listof cons/c list/c
         promise/c
         syntax/c
         
         check-between/c
         check-unary-between/c
         parameter/c
         procedure-arity-includes/c
         
         any/c
         any
         none/c
         make-none/c 

         chaperone-contract?
         impersonator-contract?
         flat-contract?
         contract?
         
         flat-contract
         flat-contract-predicate
         flat-named-contract
         
         contract-projection
         contract-name
         n->th)

(define-syntax (flat-rec-contract stx)
  (syntax-case stx  ()
    [(_ name ctc ...)
     (identifier? (syntax name))
     (with-syntax ([(ctc-id ...) (generate-temporaries (syntax (ctc ...)))]
                   [(pred-id ...) (generate-temporaries (syntax (ctc ...)))])
       (syntax 
        (let* ([pred (λ (x) (error 'flat-rec-contract "applied too soon"))]
               [name (flat-contract (let ([name (λ (x) (pred x))]) name))])
          (let ([ctc-id (coerce-contract 'flat-rec-contract ctc)] ...)
            (unless (flat-contract? ctc-id)
              (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
            ...
            (set! pred
                  (let ([pred-id (flat-contract-predicate ctc-id)] ...)
                    (λ (x)
                      (or (pred-id x) ...))))
            name))))]
    [(_ name ctc ...)
     (raise-syntax-error 'flat-rec-contract "expected first argument to be an identifier" stx (syntax name))]))

(define-syntax (flat-murec-contract stx)
  (syntax-case stx  ()
    [(_ ([name ctc ...] ...) body1 body ...)
     (andmap identifier? (syntax->list (syntax (name ...))))
     (with-syntax ([((ctc-id ...) ...) (map generate-temporaries
                                            (syntax->list (syntax ((ctc ...) ...))))]
                   [(pred-id ...) (generate-temporaries (syntax (name ...)))]
                   [((pred-arm-id ...) ...) (map generate-temporaries
                                                 (syntax->list (syntax ((ctc ...) ...))))])
       (syntax 
        (let* ([pred-id (λ (x) (error 'flat-murec-contract "applied too soon"))] ...
               [name (flat-contract (let ([name (λ (x) (pred-id x))]) name))] ...)
          (let-values ([(ctc-id ...) (values (coerce-contract 'flat-rec-contract ctc) ...)] ...)
            (begin
              (void)
              (unless (flat-contract? ctc-id)
                (error 'flat-rec-contract "expected flat contracts as arguments, got ~e" ctc-id))
              ...) ...
            (set! pred-id
                  (let ([pred-arm-id (flat-contract-predicate ctc-id)] ...)
                    (λ (x)
                      (or (pred-arm-id x) ...)))) ...
            body1
            body ...))))]
    [(_ ([name ctc ...] ...) body1 body ...)
     (for-each (λ (name)
                 (unless (identifier? name)
                   (raise-syntax-error 'flat-rec-contract
                                       "expected an identifier" stx name)))
               (syntax->list (syntax (name ...))))]
    [(_ ([name ctc ...] ...))
     (raise-syntax-error 'flat-rec-contract "expected at least one body expression" stx)]))

(define/subexpression-pos-prop or/c
  (case-lambda 
    [() (make-none/c '(or/c))]
    [raw-args
     (let ([args (coerce-contracts 'or/c raw-args)])
       (let-values ([(ho-contracts flat-contracts)
                     (let loop ([ho-contracts '()]
                                [flat-contracts '()]
                                [args args])
                       (cond
                         [(null? args) (values ho-contracts (reverse flat-contracts))]
                         [else 
                          (let ([arg (car args)])
                            (cond
                              [(flat-contract? arg)
                               (loop ho-contracts (cons arg flat-contracts) (cdr args))]
                              [else
                               (loop (cons arg ho-contracts) flat-contracts (cdr args))]))]))])
         (let ([pred 
                (cond
                  [(null? flat-contracts) not]
                  [else
                   (let loop ([fst (car flat-contracts)]
                              [rst (cdr flat-contracts)])
                     (let ([fst-pred (flat-contract-predicate fst)])
                       (cond
                         [(null? rst) fst-pred]
                         [else 
                          (let ([r (loop (car rst) (cdr rst))])
                            (λ (x) (or (fst-pred x) (r x))))])))])])
           (cond
             [(null? ho-contracts)
              (make-flat-or/c pred flat-contracts)]
             [(null? (cdr ho-contracts))
              (if (chaperone-contract? (car ho-contracts))
                  (make-chaperone-single-or/c pred flat-contracts (car ho-contracts))
                  (make-impersonator-single-or/c pred flat-contracts (car ho-contracts)))]
             [else
              (if (andmap chaperone-contract? ho-contracts)
                  (make-chaperone-multi-or/c flat-contracts ho-contracts)
                  (make-impersonator-multi-or/c flat-contracts ho-contracts))]))))]))

;; Generator for or/c contracts
(define (or/c-generate ctcs)
  (define filtered-ctcs (filter (λ (c) (not (contract-stronger? c none/c)))
                                ctcs))
  ;; Try to generate a random value from  the or/c and pick one of the results
  ;; at random. Fail if all of the results produce generate-ctc-fail
  (define (generate-all fuel)
    (let ([perm-seq (sequence-map (λ (c) (generate/choose c fuel))
                                  (in-list (permute filtered-ctcs)))])
      (let-values ([(more? get-next) (sequence-generate perm-seq)])
        (let loop ()
          (cond [(more?)
                  (let ([next (get-next)])
                    (if (generate-ctc-fail? next)
                      (loop)
                      next))]
                [else (generate-ctc-fail ctcs)])))))
  (λ (fuel)
     (cond [(null? filtered-ctcs) (generate-ctc-fail none/c)]
           [else (generate-all (/ fuel 2))])))

;; Exerciser for or/c contracts
(define (or/c-exercise flat-ctcs ho-ctcs)
  ;(eprintf "flats ~s ho-ctcs ~s\n" flat-ctcs ho-ctcs)
  (λ (val fuel)
     (let loop ([flats flat-ctcs]
                [hos ho-ctcs]
                [missing #f]
                [failed #f])
       (define (check-ho ctc)
         (let ([exerciser (contract-struct-exercise ctc)])
           (cond [(procedure? exerciser) (exerciser val (/ fuel 2))]
                 [else (loop flats (cdr hos) (cons ctc (or missing null)) #f)])))
       (cond [(null? flats)
              (cond [(not (null? hos)) (check-ho (car hos))]
                    [missing (exercise-fail missing "no exerciser found")]
                    [failed (exercise-fail failed "exercise failed")])]
             [((flat-contract-predicate (car flats)) val) (void)]
             [else (loop (cdr flats) val)]))))

(define (single-or/c-projection ctc)
  (let ([c-proc (contract-projection (single-or/c-ho-ctc ctc))]
        [pred (single-or/c-pred ctc)])
    (λ (blame)
      (define partial-contract
        (c-proc (blame-add-context blame "a disjunct of")))
      (λ (val)
        (cond
          [(pred val) val]
          [else (partial-contract val)])))))

(define (single-or/c-name ctc)
  (apply build-compound-type-name 
         'or/c 
         (single-or/c-ho-ctc ctc)
         (single-or/c-flat-ctcs ctc)))

(define (single-or/c-first-order ctc)
  (let ([pred (single-or/c-pred ctc)]
        [ho (contract-first-order (single-or/c-ho-ctc ctc))])
    (λ (x) (or (ho x) (pred x)))))

(define (single-or/c-stronger? this that)
  (and (single-or/c? that)
       (contract-stronger? (single-or/c-ho-ctc this)
                           (single-or/c-ho-ctc that))
       (let ([this-ctcs (single-or/c-flat-ctcs this)]
             [that-ctcs (single-or/c-flat-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger?
                      this-ctcs
                      that-ctcs)))))

(define (single-or/c-generate ctc)
  (or/c-generate (cons (single-or/c-ho-ctc ctc)
                       (single-or/c-flat-ctcs ctc))))

(define (single-or/c-exercise ctc)
  (or/c-exercise (single-or/c-flat-ctcs ctc)
                 (list (single-or/c-ho-ctc ctc))))

(define-struct single-or/c (pred flat-ctcs ho-ctc))

(define-struct (chaperone-single-or/c single-or/c) ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:projection single-or/c-projection
     #:name single-or/c-name
     #:first-order single-or/c-first-order
     #:stronger single-or/c-stronger?
     #:generate single-or/c-generate
     #:exercise single-or/c-exercise)))

(define-struct (impersonator-single-or/c single-or/c) ()
  #:property prop:contract
  (build-contract-property
   #:projection single-or/c-projection
   #:name single-or/c-name
   #:first-order single-or/c-first-order
   #:stronger single-or/c-stronger?
   #:generate single-or/c-generate
   #:exercise single-or/c-exercise))

(define (multi-or/c-proj ctc)
  (let* ([ho-contracts (multi-or/c-ho-ctcs ctc)]
         [c-procs (map (λ (x) (contract-projection x)) ho-contracts)]
         [first-order-checks (map (λ (x) (contract-first-order x)) ho-contracts)]
         [predicates (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))])
    (λ (blame)
      (define disj-blame (blame-add-context blame "a disjunct of"))
      (define partial-contracts
        (for/list ([c-proc (in-list c-procs)])
          (c-proc disj-blame)))
      (λ (val)
        (cond
          [(ormap (λ (pred) (pred val)) predicates)
           val]
          [else
           (let loop ([checks first-order-checks]
                      [procs partial-contracts]
                      [contracts ho-contracts]
                      [candidate-proc #f]
                      [candidate-contract #f])
             (cond
               [(null? checks)
                (if candidate-proc
                    (candidate-proc val)
                    (raise-blame-error blame val 
                                       '("none of the branches of the or/c matched," given: "~e")
                                       val))]
               [((car checks) val)
                (if candidate-proc
                    (raise-blame-error blame val
                                       '("two of the clauses in the or/c might both match: ~s and ~s," given: "~e")
                                       (contract-name candidate-contract)
                                       (contract-name (car contracts))
                                       val)
                    (loop (cdr checks)
                          (cdr procs)
                          (cdr contracts)
                          (car procs)
                          (car contracts)))]
               [else
                (loop (cdr checks)
                      (cdr procs)
                      (cdr contracts)
                      candidate-proc
                      candidate-contract)]))])))))

(define (multi-or/c-name ctc)
  (apply build-compound-type-name 
         'or/c 
         (append
          (multi-or/c-flat-ctcs ctc)
          (reverse (multi-or/c-ho-ctcs ctc)))))

(define (multi-or/c-first-order ctc)
  (let ([flats (map flat-contract-predicate (multi-or/c-flat-ctcs ctc))]
        [hos (map (λ (x) (contract-first-order x)) (multi-or/c-ho-ctcs ctc))])
    (λ (x)
      (or (ormap (λ (f) (f x)) hos)
          (ormap (λ (f) (f x)) flats)))))

(define (multi-or/c-stronger? this that)
  (and (multi-or/c? that)
       (let ([this-ctcs (multi-or/c-ho-ctcs this)]
             [that-ctcs (multi-or/c-ho-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger? this-ctcs that-ctcs)))
       (let ([this-ctcs (multi-or/c-flat-ctcs this)]
             [that-ctcs (multi-or/c-flat-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger? this-ctcs that-ctcs)))))

(define (multi-or/c-generate ctc)
  (or/c-generate (append (multi-or/c-flat-ctcs ctc)
                         (multi-or/c-ho-ctcs ctc))))

(define (multi-or/c-exercise ctc)
  (or/c-exercise (multi-or/c-flat-ctcs ctc)
                 (multi-or/c-ho-ctcs ctc)))

(define-struct multi-or/c (flat-ctcs ho-ctcs))

(define-struct (chaperone-multi-or/c multi-or/c) ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:projection multi-or/c-proj
     #:name multi-or/c-name
     #:first-order multi-or/c-first-order
     #:stronger multi-or/c-stronger?
     #:generate multi-or/c-generate
     #:exercise multi-or/c-exercise)))

(define-struct (impersonator-multi-or/c multi-or/c) ()
  #:property prop:contract
  (build-contract-property
   #:projection multi-or/c-proj
   #:name multi-or/c-name
   #:first-order multi-or/c-first-order
   #:stronger multi-or/c-stronger?
   #:generate multi-or/c-generate
   #:exercise multi-or/c-exercise))

(define-struct flat-or/c (pred flat-ctcs)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc)
      (apply build-compound-type-name 
             'or/c 
             (flat-or/c-flat-ctcs ctc)))
   #:stronger
   (λ (this that)
      (and (flat-or/c? that)
           (let ([this-ctcs (flat-or/c-flat-ctcs this)]
                 [that-ctcs (flat-or/c-flat-ctcs that)])
             (cond
               [(and (<= (length this-ctcs) (length that-ctcs))
                     (for/and ([this-ctc (in-list this-ctcs)]
                               [that-ctc (in-list that-ctcs)])
                       (contract-stronger? this-ctc that-ctc)))
                #t]
               [(and (andmap (λ (x) (or (eq-contract? x) (equal-contract? x))) this-ctcs)
                     (andmap (λ (x) (or (eq-contract? x) (equal-contract? x))) that-ctcs))
                (define ht (make-hash))
                (for ([x (in-list that-ctcs)])
                  (hash-set! ht
                             (if (equal-contract? x)
                                 (equal-contract-val x)
                                 (eq-contract-val x))
                             #t))
                (for/and ([x (in-list this-ctcs)])
                  (hash-ref ht 
                            (if (equal-contract? x)
                                (equal-contract-val x)
                                (eq-contract-val x))
                            #f))]
               [else #f]))))
                    

   #:first-order
   (λ (ctc) (flat-or/c-pred ctc))
   #:generate
   (λ (ctc) (or/c-generate (flat-or/c-flat-ctcs ctc)))
   #:exercise
   (λ (ctc) (or/c-exercise (flat-or/c-flat-ctcs ctc) null))
   ))


(define (and-name ctc)
  (apply build-compound-type-name 'and/c (base-and/c-ctcs ctc)))

(define (and-first-order ctc)
  (let ([tests (map contract-first-order (base-and/c-ctcs ctc))])
    (λ (x) (for/and ([test (in-list tests)]) (test x)))))

(define (and-proj ctc)
  (let ([mk-pos-projs (map contract-projection (base-and/c-ctcs ctc))])
    (lambda (blame)
      (define projs 
        (for/list ([c (in-list mk-pos-projs)]
                   [n (in-naturals 1)])
          (c (blame-add-context blame (format "the ~a conjunct of" (n->th n))))))
      (for/fold ([proj (car projs)])
        ([p (in-list (cdr projs))])
        (λ (v) (p (proj v)))))))

(define (first-order-and-proj ctc)
  (λ (blame)
    (λ (val)
      (let loop ([predicates (first-order-and/c-predicates ctc)]
                 [ctcs (base-and/c-ctcs ctc)])
        (cond
          [(null? predicates) val]
          [else
           (if ((car predicates) val)
               (loop (cdr predicates) (cdr ctcs))
               (raise-blame-error
                blame
                val
                '(expected: "~s," given: "~e, which isn't ~s")
                (contract-name ctc)
                val
                (contract-name (car ctcs))))])))))

(define (and-stronger? this that)
  (and (base-and/c? that)
       (let ([this-ctcs (base-and/c-ctcs this)]
             [that-ctcs (base-and/c-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger?
                      this-ctcs
                      that-ctcs)))))

(define (and/c-generate ctc)
  (λ (fuel)
     ; Try to find the strongest contract
     (define ((is-strongest? cs) c)
       (andmap (λ (that) (contract-stronger? c that)) cs))
     (let* ([fuel (- fuel 1)]
            [all-ctcs (base-and/c-ctcs ctc)]
            [options (filter (is-strongest? all-ctcs) all-ctcs)])
       (let loop ([options (permute options)])
         (cond [(null? options) (generate-ctc-fail ctc)]
               [else (let ([val (generate/choose (car options) fuel)])
                       (cond [(generate-ctc-fail? val) (loop (cdr options))]
                             [else val]))])))))

(define (and/c-exercise ctc)
  (λ (val fuel print-gen)
     (let ([all-ctcs (base-and/c-ctcs ctc)])
       (for ([c all-ctcs])
         (contract-random-exercise c val fuel print-gen)))))


(define-struct base-and/c (ctcs))
(define-struct (first-order-and/c base-and/c) (predicates)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection first-order-and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate
   #:exercise and/c-exercise))

(define-struct (chaperone-and/c base-and/c) ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:projection and-proj
     #:name and-name
     #:first-order and-first-order
     #:stronger and-stronger?
     #:generate and/c-generate
     #:exercise and/c-exercise)))
(define-struct (impersonator-and/c base-and/c) ()
  #:property prop:contract
  (build-contract-property
   #:projection and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?
   #:generate and/c-generate
   #:exercise and/c-exercise))


(define/subexpression-pos-prop (and/c . raw-fs)
  (let ([contracts (coerce-contracts 'and/c raw-fs)])
    (cond
      [(null? contracts) any/c]
      [(andmap flat-contract? contracts)
       (let ([preds (map flat-contract-predicate contracts)])
         (make-first-order-and/c contracts preds))]
      [(andmap chaperone-contract? contracts)
       (make-chaperone-and/c contracts)]
      [else (make-impersonator-and/c contracts)])))


(define false/c #f)

(define/final-prop (string-len/c n)
  (unless (number? n)
    (error 'string-len/c "expected a number as argument, got ~e" n))
  (flat-named-contract 
   `(string-len/c ,n)
   (λ (x)
     (and (string? x)
          ((string-length x) . < . n)))))

(define/final-prop (symbols . ss)
  (unless ((length ss) . >= . 1)
    (error 'symbols "expected at least one argument"))
  (unless (andmap symbol? ss)
    (error 'symbols "expected symbols as arguments, given: ~a"
           (apply string-append (map (λ (x) (format "~e " x)) ss))))
  (apply or/c ss))

(define atomic-value? 
  (let ([undefined (letrec ([x x]) x)])
    (λ (x)
      (or (char? x) (symbol? x) (boolean? x)
          (null? x) (keyword? x) (number? x)
          (void? x) (eq? x undefined)))))

(define/final-prop (one-of/c . elems)
  (unless (andmap atomic-value? elems)
    (error 'one-of/c "expected chars, symbols, booleans, null, keywords, numbers, void, or undefined, got ~e"
           elems))
  (if (or (member (void) elems)
          (member (letrec ([x x]) x) elems))
      (make-one-of/c elems)
      (apply or/c elems)))

(define (one-of-pc x)
  (cond
    [(symbol? x)
     `',x]
    [(null? x)
     ''()]
    [(void? x)
     '(void)]
    [(or (char? x) 
         (boolean? x)
         (keyword? x)
         (number? x))
     x]
    [(eq? x (letrec ([x x]) x))
     '(letrec ([x x]) x)]
    [else (error 'one-of-pc "undef ~s" x)]))


(define-struct one-of/c (elems)
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc) 
      (let ([elems (one-of/c-elems ctc)])
        `(one-of/c ,@(map one-of-pc elems))))

   #:stronger
   (λ (this that)
      (and (one-of/c? that)
           (let ([this-elems (one-of/c-elems this)]
                 [that-elems (one-of/c-elems that)])
             (and 
              (andmap (λ (this-elem) (memv this-elem that-elems))
                      this-elems)
              #t))))
   #:first-order 
   (λ (ctc) 
      (let ([elems (one-of/c-elems ctc)])
        (λ (x) (memv x elems))))))

(define-struct between/c (low high)
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc) 
      (let ([n (between/c-low ctc)]
            [m (between/c-high ctc)])
        (cond
          [(and (= n -inf.0) (= m +inf.0))
           `(between/c ,n ,m)]
          [(= n -inf.0) `(<=/c ,m)]
          [(= m +inf.0) `(>=/c ,n)]
          [(= n m) `(=/c ,n)]
          [else `(between/c ,n ,m)])))

   #:stronger
   (λ (this that)
      (and (between/c? that)
           (<= (between/c-low that) (between/c-low this))
           (<= (between/c-high this) (between/c-high that))))

   #:first-order
   (λ (ctc) 
      (let ([n (between/c-low ctc)]
            [m (between/c-high ctc)])
        (λ (x) 
           (and (real? x)
                (<= n x m)))))
   #:generate
   (λ (ctc)
      (λ (fuel)
         (let* ([max-n 2147483647]
                [min-n -2147483648]
                [upper (if (> (between/c-high ctc) max-n)
                         max-n
                         (between/c-high ctc))]
                [lower (if (< (between/c-low ctc) min-n)
                         min-n
                         (between/c-low ctc))])
           (rand-range lower upper))))))

(define-syntax (check-unary-between/c stx)
  (syntax-case stx ()
    [(_ 'sym x-exp)
     (identifier? #'sym)
     #'(let ([x x-exp])
         (unless (real? x)
           (error 'sym "expected a real number, got ~e" x)))]))

(define/final-prop (=/c x) 
  (check-unary-between/c '=/c x)
  (make-between/c x x))
(define/final-prop (<=/c x) 
  (check-unary-between/c '<=/c x)
  (make-between/c -inf.0 x))
(define/final-prop (>=/c x)
  (check-unary-between/c '>=/c x)
  (make-between/c x +inf.0))
(define (check-between/c x y)
  (unless (real? x)
    (error 'between/c "expected a real number as first argument, got ~e, other arg ~e" x y))
  (unless (real? y)
    (error 'between/c "expected a real number as second argument, got ~e, other arg ~e" y x)))
(define/final-prop (between/c x y)
  (check-between/c x y)
  (make-between/c x y))


(define (</c x)
  (flat-named-contract
   `(</c ,x)
   (λ (y) (and (real? y) (< y x)))
   (λ (fuel)
      (let* ([max-n 2147483647]
             [min-n -2147483648]
             [upper (if (> x max-n)
                      max-n
                      x)])
        (rand-range min-n upper)))))

(define (>/c x)
  (flat-named-contract
    `(>/c ,x)
    (λ (y) (and (real? y) (> y x)))
    (λ (fuel) 
       (let* ([max-n 2147483647]
              [min-n -2147483648]
              [lower (if (< x min-n)
                       min-n
                       x)])
         (rand-range lower max-n)))))

(define/final-prop (integer-in start end)
  (unless (and (integer? start)
               (exact? start)
               (integer? end)
               (exact? end))
    (error 'integer-in "expected two exact integers as arguments, got ~e and ~e" start end))
  (flat-named-contract 
   `(integer-in ,start ,end)
   (λ (x)
     (and (integer? x)
          (exact? x)
          (<= start x end)))))

(define/final-prop (real-in start end)
  (unless (and (real? start)
               (real? end))
    (error 'real-in "expected two real numbers as arguments, got ~e and ~e" start end))
  (between/c start end))

(define/final-prop (not/c f)
  (let* ([ctc (coerce-flat-contract 'not/c f)]
         [pred (flat-contract-predicate ctc)])
    (flat-named-contract
     (build-compound-type-name 'not/c ctc)
     (λ (x) (not (pred x))))))

(define (listof-generate elem-ctc)
  (λ (fuel)
     (define (mk-rand-list so-far)
       (rand-choice
         [1/5 so-far]
         [else (let ([elem (generate/choose elem-ctc fuel)])
                 (if (generate-ctc-fail? elem)
                     elem
                     (mk-rand-list (cons elem so-far))))]))
     (mk-rand-list (list))))

(define-syntax (*-listof stx)
  (syntax-case stx ()
    [(_ predicate? type-name name generate)
     (identifier? (syntax predicate?))
     (syntax
      (λ (input)
        (let* ([ctc (coerce-contract 'name input)]
               [ctc-name (build-compound-type-name 'name ctc)]
               [proj (contract-projection ctc)])
          (define (fo-check x)
            (and (predicate? x) 
                 (for/and ([v (in-list x)])
                   (contract-first-order-passes? ctc v))))
          (define ((ho-check check-all) blame)
            (let ([p-app (proj (blame-add-context blame "an element of"))])
              (λ (val)
                (unless (predicate? val)
                  (raise-blame-error blame val
                                     '(expected: "~s," given "~e")
                                     'type-name 
                                     val))
                (check-all p-app val))))
          (cond
            [(flat-contract? ctc)
             (make-flat-contract
              #:name ctc-name
              #:first-order fo-check
              #:projection (ho-check (λ (p v) (for-each p v) v))
              #:generate (generate ctc))]
            [(chaperone-contract? ctc)
             (make-chaperone-contract
              #:name ctc-name
              #:first-order fo-check
              #:projection (ho-check (λ (p v) (map p v)))
              #:generate (generate ctc))]
            [else
             (make-contract
              #:name ctc-name
              #:first-order fo-check
              #:projection (ho-check (λ (p v) (map p v)))
              #:generate (generate ctc))]))))]))

(define listof-func (*-listof list? list listof listof-generate))
(define/subexpression-pos-prop (listof x) (listof-func x))

(define (non-empty-list? x) (and (pair? x) (list? (cdr x))))
(define non-empty-listof-func (*-listof 
                                non-empty-list? 
                                non-empty-list 
                                non-empty-listof 
                                (λ (ctc) (generate-ctc-fail ctc))))
(define/subexpression-pos-prop (non-empty-listof a) (non-empty-listof-func a))

(define cons/c-main-function
  (λ (car-c cdr-c)
    (let* ([ctc-car (coerce-contract 'cons/c car-c)]
           [ctc-cdr (coerce-contract 'cons/c cdr-c)]
           [ctc-name (build-compound-type-name 'cons/c ctc-car ctc-cdr)]
           [car-proj (contract-projection ctc-car)]
           [cdr-proj (contract-projection ctc-cdr)])
      (define (fo-check v)
        (and (pair? v)
             (contract-first-order-passes? ctc-car (car v))
             (contract-first-order-passes? ctc-cdr (cdr v))))
      (define ((ho-check combine) blame)
        (let ([car-p (car-proj (blame-add-context blame "the car of"))]
              [cdr-p (cdr-proj (blame-add-context blame "the cdr of"))])
          (λ (v)
            (unless (pair? v)
              (raise-blame-error blame v 
                                 '(expected "<pair?>," given: "~e")
                                 v))
            (combine v (car-p (car v)) (cdr-p (cdr v))))))
      (cond
        [(and (flat-contract? ctc-car) (flat-contract? ctc-cdr))
         (make-flat-contract
          #:name ctc-name
          #:first-order fo-check
          #:projection (ho-check (λ (v a d) v)))]
        [(and (chaperone-contract? ctc-car) (chaperone-contract? ctc-cdr))
         (make-chaperone-contract
          #:name ctc-name
          #:first-order fo-check
          #:projection (ho-check (λ (v a d) (cons a d))))]
        [else
         (make-contract
           #:name ctc-name
           #:first-order fo-check
           #:projection (ho-check (λ (v a d) (cons a d))))]))))

(define/subexpression-pos-prop (cons/c a b) (cons/c-main-function a b))

(define/subexpression-pos-prop (list/c . args)
  (define ctc-args (coerce-contracts 'list/c args))
  (cond
    [(andmap flat-contract? ctc-args)
     (flat-list/c ctc-args)]
    [(andmap chaperone-contract? ctc-args)
     (chaperone-list/c ctc-args)]
    [else
     (higher-order-list/c ctc-args)]))

(define (list/c-name-proc c) 
  (apply build-compound-type-name
         'list/c (generic-list/c-args c)))
(define ((list/c-first-order c) x)
  (and (list? x)
       (= (length x) (length (generic-list/c-args c)))
       (for/and ([arg/c (in-list (generic-list/c-args c))]
                 [v (in-list x)])
         ((contract-first-order arg/c) v))))

(struct generic-list/c (args))

(struct flat-list/c generic-list/c ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name list/c-name-proc
   #:first-order list/c-first-order
   #:projection
   (lambda (c)
     (lambda (blame)
       (lambda (x)
         (unless (list? x)
           (raise-blame-error blame x '(expected "a list," given: "~e") x))
         (let* ([args (generic-list/c-args c)]
                [expected (length args)]
                [actual (length x)])
           (unless (= actual expected)
             (raise-blame-error
              blame x
              '(expected "a list of ~a elements, but" given "~a element~a in: ~e")
              expected 
              actual
              (if (= actual 1) "" "s")
              x))
           (for ([arg/c (in-list args)] [v (in-list x)] [i (in-naturals 1)])
             (((contract-projection arg/c) 
               (add-list-context blame i))
              v))
           x))))))

(define (list/c-chaperone/other-projection c)
  (define args (map contract-projection (generic-list/c-args c)))
  (define expected (length args))
  (λ (blame)
    (define projs (for/list ([arg/c (in-list args)]
                             [i (in-naturals 1)])
                    (arg/c (add-list-context blame i))))
    (λ (x)
      (unless (list? x)
        (raise-blame-error blame x '(expected "a list," given: "~e") x))
      (define actual (length x))
      (unless (= actual expected)
        (raise-blame-error
         blame x
         '(expected "a list of ~a elements, but" given "~a element~a in: ~e")
         expected
         actual
         (if (= actual 1) "" "s")
         x))
      (for/list ([item (in-list x)]
                 [proj (in-list projs)])
        (proj item)))))

(define (add-list-context blame i)
  (blame-add-context blame (format "the ~a~a element of"
                                   i
                                   (case (modulo i 10)
                                     [(1) "st"]
                                     [(2) "nd"]
                                     [(3) "rd"]
                                     [else "th"]))))

(struct chaperone-list/c generic-list/c ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:name list/c-name-proc
     #:first-order list/c-first-order
     #:projection list/c-chaperone/other-projection)))

(struct higher-order-list/c generic-list/c ()
  #:property prop:contract
  (build-contract-property
   #:name list/c-name-proc
   #:first-order list/c-first-order
   #:projection list/c-chaperone/other-projection))

(define/subexpression-pos-prop (syntax/c ctc-in)
  (let ([ctc (coerce-contract 'syntax/c ctc-in)])
    (flat-named-contract
     (build-compound-type-name 'syntax/c ctc)
     (let ([pred (flat-contract-predicate ctc)])
       (λ (val)
         (and (syntax? val)
              (pred (syntax-e val))))))))

(define/subexpression-pos-prop promise/c
  (λ (ctc-in)
    (let* ([ctc (coerce-contract 'promise/c ctc-in)]
           [ctc-proc (contract-projection ctc)])
      (make-contract
       #:name (build-compound-type-name 'promise/c ctc)
       #:projection
       (λ (blame)
         (let ([p-app (ctc-proc (blame-add-context blame "the promise from"))])
           (λ (val)
             (unless (promise? val)
               (raise-blame-error
                blame
                val
                '(expected "<promise>," given: "~e")
                val))
             (delay (p-app (force val))))))
       #:first-order promise?
       #:generate
       (λ (fuel) (delay (generate/choose ctc fuel)))
       ))))

(define/subexpression-pos-prop (parameter/c x)
  (make-parameter/c (coerce-contract 'parameter/c x)))

(define-struct parameter/c (ctc)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
      (let ([c-proc (contract-projection (parameter/c-ctc ctc))])
        (λ (blame)
          (define blame/c (blame-add-context blame "the parameter of"))
          (define partial-neg-contract (c-proc (blame-swap blame/c)))
          (define partial-pos-contract (c-proc blame/c))
          (λ (val)
            (cond
              [(parameter? val)
               (make-derived-parameter 
                val 
                partial-neg-contract
                partial-pos-contract)]
              [else
               (raise-blame-error blame val '(expected "a parameter"))])))))

   #:name
   (λ (ctc) (build-compound-type-name 'parameter/c (parameter/c-ctc ctc)))
   #:first-order
   (λ (ctc)
      (let ([tst (contract-first-order (parameter/c-ctc ctc))])
        (λ (x)
           (and (parameter? x)
                (tst (x))))))

   #:stronger
   (λ (this that)
      ;; must be invariant (because the library doesn't currently split out pos/neg contracts
      ;; which could be tested individually ....)
      (and (parameter/c? that)
           (contract-stronger? (parameter/c-ctc this) 
                               (parameter/c-ctc that))
           (contract-stronger? (parameter/c-ctc that) 
                               (parameter/c-ctc this))))))

(define-struct procedure-arity-includes/c (n)
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:stronger (λ (this that) (and (procedure-arity-includes/c? that)
                                  (= (procedure-arity-includes/c-n this)
                                     (procedure-arity-includes/c-n that))))
   #:name (λ (ctc) `(procedure-arity-includes/c ,(procedure-arity-includes/c-n ctc)))
   #:first-order (λ (ctc)
                   (define n (procedure-arity-includes/c-n ctc))
                   (λ (x)
                     (and (procedure? x)
                          (procedure-arity-includes? x n))))))

(define/subexpression-pos-prop (procedure-arity-includes/c n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'procedure-arity-includes/c
                          "exact-nonnegative-integer?"
                          n))
  (make-procedure-arity-includes/c n))

(define (get-any-projection c) any-projection)
(define (any-projection b) any-function)
(define (any-function x) x)

(define (get-any? c) any?)
(define (any? x) #t)

(define-struct any/c ()
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection get-any-projection
   #:stronger (λ (this that) (any/c? that))
   #:name (λ (ctc) 'any/c)
   #:first-order get-any?
   #:generate
   (λ (ctc)
      (λ (fuel)
         (let ([c (generate/direct contract? fuel)])
           (generate/choose c fuel))))
   ))

(define/final-prop any/c (make-any/c))

(define-syntax (any stx)
  (raise-syntax-error 'any "use of 'any' outside the range of an arrow contract" stx))

(define (none-curried-proj ctc)
  (λ (blame)
    (λ (val) 
      (raise-blame-error
       blame
       val
       '("~s accepts no values," given: "~e")
       (none/c-name ctc)
       val))))

(define-struct none/c (name)
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection none-curried-proj
   #:stronger (λ (this that) #t)
   #:name (λ (ctc) (none/c-name ctc))
   #:first-order (λ (ctc) (λ (val) #f))))

(define/final-prop none/c (make-none/c 'none/c))


(define (flat-contract-predicate x)
  (contract-struct-first-order
   (coerce-flat-contract 'flat-contract-predicate x)))

(define (flat-contract? x) 
  (let ([c (coerce-contract/f x)])
    (and c
         (flat-contract-struct? c))))

(define (chaperone-contract? x)
  (let ([c (coerce-contract/f x)])
    (and c
         (or (chaperone-contract-struct? c)
             (and (prop:opt-chaperone-contract? c)
                  ((prop:opt-chaperone-contract-get-test c) c))))))

(define (impersonator-contract? x)
  (let ([c (coerce-contract/f x)])
    (and c
         (not (flat-contract-struct? c))
         (not (chaperone-contract-struct? c)))))

(define (contract-name ctc)
  (contract-struct-name
   (coerce-contract 'contract-name ctc)))

(define (contract? x) (and (coerce-contract/f x) #t))
(define (contract-projection ctc)
  (contract-struct-projection
   (coerce-contract 'contract-projection ctc)))

;; Add predicate generator for contract?
(contract-add-generate 
  contract?
  (λ (fuel)
     (let ([sample (oneof (get-env-contracts))])
       (cond [(contract? sample) sample]
             [else (coerce-contract 'contract?-generate sample)]))))

(define (flat-contract predicate) (coerce-flat-contract 'flat-contract predicate))
(define (flat-named-contract name predicate [generate (generate-ctc-fail #f)] [exercise #f])
  (cond
    [(and (procedure? predicate)
          (procedure-arity-includes? predicate 1))
     (make-predicate-contract name predicate generate)]
    [(flat-contract? predicate)
     (make-predicate-contract name (flat-contract-predicate predicate) generate)]
    [else
     (error 'flat-named-contract 
            "expected a flat contract or procedure of arity 1 as second argument, got ~e" 
            predicate)]))



(define printable/c
  (flat-named-contract
   'printable/c
   (λ (x)
     (let printable? ([x x])
       (or (symbol? x)
           (string? x)
           (bytes? x)
           (boolean? x)
           (char? x)
           (null? x)
           (number? x)
           (regexp? x)
           (prefab-struct-key x) ;; this cannot be last, since it doesn't return just #t
           (and (pair? x)
                (printable? (car x))
                (printable? (cdr x)))
           (and (vector? x)
                (andmap printable? (vector->list x)))
           (and (box? x)
                (printable? (unbox x))))))))


(define natural-number/c
  (flat-named-contract
   'natural-number/c
   (λ (x)
     (and (number? x)
          (integer? x)
          (exact? x)
          (x . >= . 0)))))

(define (n->th n)
  (string-append 
   (number->string n)
   (case (modulo n 10)
     [(1) "st"]
     [(2) "nd"]
     [(3) "rd"]
     [else "th"])))

