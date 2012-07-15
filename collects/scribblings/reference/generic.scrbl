#lang scribble/manual
@(require (for-label racket/base racket/generic))

@title[#:tag "struct-generics"]{Generic Interfaces}
@; @author[@author+email["Eli Barzilay" "eli@racket-lang.org"]
@;         @author+email["Jay McCarthy" "jay@racket-lang.org"]
@; 	@author+email["Vincent St-Amour" "stamourv@racket-lang.org"]
@; 	@author+email["Asumu Takikawa" "asumu@racket-lang.org"]]

@defmodule[racket/generic]

A @deftech{generic interface} allows per-type methods to be
associated with generic functions. Generic functions are defined
using a @racket[define-generics] form. Method implementations for
a structure type are defined using the @racket[#:methods] keyword
(see @secref["define-struct"]).

@defform/subs[(define-generics id [#:defined-table defined-table-id]
                [method-id . kw-formals*]
                ...)
              ([kw-formals* (arg* ...)
                            (arg* ...+ . rest-id)
                            rest-id]
               [arg* arg-id
                     [arg-id]
                     (code:line keyword arg-id)
                     (code:line keyword [arg-id])])]{

Defines 

@itemlist[

 @item{@racketidfont{gen:}@racket[id] as a transformer binding for
       the static information about a new generic interface;}

 @item{@racket[id]@racketidfont{?} as a predicate identifying
       instances of structure types that implement this generic group; and}

 @item{each @racket[method-id] as a generic procedure that calls the
       corresponding method on values where
       @racket[id]@racketidfont{?} is true.}

]

Each @racket[method-id]'s @racket[kw-formals*] must include a required
by-position argument that is @racket[free-identifier=?] to
@racket[id]. That argument is used in the generic definition to
locate the specialization.

When @racket[defined-table-id] is provided, it is defined as a
procedure that takes an instance of the generics and returns an
immutable @tech{hash table} that maps symbols corresponding to method
names to booleans representing whether or not that method is
implemented by the instance. This table is intended for use by
higher-level APIs to adapt their behavior depending on method
availability.}


@defform[(define/generic local-id method-id)
         #:contracts
         ([local-id identifier?]
          [method-id identifier?])]{

When used inside the method definitions associated with the
@racket[#:methods] keyword, binds @racket[local-id] to the generic for
@racket[method-id]. This form is useful for method specializations to
use generic methods (as opposed to the local specialization) on other
values.

Using the @racket[define/generic] form outside a @racket[#:methods]
specification in @racket[struct] (or @racket[define-struct]) is an
syntax error.}


@; Examples
@(require scribble/eval)
@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax racket/base)
                  racket/generic))
     e))

@(define evaluator (new-evaluator))

@examples[#:eval evaluator
(define-generics printable
  (gen-print printable [port])
  (gen-port-print port printable)
  (gen-print* printable [port] #:width width #:height [height]))

(define-struct num (v)
  #:methods gen:printable
  [(define/generic super-print gen-print)
   (define (gen-print n [port (current-output-port)])
     (fprintf port "Num: ~a" (num-v n)))
   (define (gen-port-print port n)
     (super-print n port))
   (define (gen-print* n [port (current-output-port)]
                       #:width w #:height [h 0])
     (fprintf port "Num (~ax~a): ~a" w h (num-v n)))])
         
(define-struct bool (v)
  #:methods gen:printable
  [(define/generic super-print gen-print)
   (define (gen-print b [port (current-output-port)])
     (fprintf port "Bool: ~a" 
              (if (bool-v b) "Yes" "No")))
   (define (gen-port-print port b)
     (super-print b port))
   (define (gen-print* b [port (current-output-port)]
                       #:width w #:height [h 0])
     (fprintf port "Bool (~ax~a): ~a" w h 
              (if (bool-v b) "Yes" "No")))])

(define x (make-num 10))
(gen-print x)
(gen-port-print (current-output-port) x)
(gen-print* x #:width 100 #:height 90)

(define y (make-bool #t))
(gen-print y)
(gen-port-print (current-output-port) y)
(gen-print* y #:width 100 #:height 90)
]

@close-eval[evaluator]