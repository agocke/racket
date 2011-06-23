#lang racket/base

(require (for-syntax racket/base))
(require (for-meta 2 racket/base))
(require racket/class)

(require "private/honu-typed-scheme.rkt"
         "private/parse.ss"
         (for-syntax "private/literals.rkt")
         (for-syntax "private/honu-typed-scheme.rkt")
         (for-syntax "private/parse.rkt")
         (for-syntax "private/canonical.rkt")
         syntax/parse
         (for-syntax syntax/parse)
         "private/literals.rkt"
         "private/syntax.rkt"
         "private/more.rkt"
         (for-template racket/base)
         (for-template "private/literals.rkt")
         (for-syntax "private/more.rkt")
         (for-syntax "private/syntax.rkt")
         (for-syntax "private/macro.rkt")
         "private/macro.ss")

(define-for-syntax (syntax-to-string stx)
  (format "original '~a' - ~a" (syntax->datum stx) (to-honu-string stx)))

(define-syntax (honu-struct stx)
  (syntax-parse stx
    [(_ name (my-field ...))
     (with-syntax ([new-name (gensym (syntax->datum #'name))])
       #'(begin
           (define new-name
             (class object%
                    (init-field my-field ...)
                    (super-new)))
           (define name (lambda args (apply make-object new-name args)))))]))

(provide (rename-out (#%dynamic-honu-module-begin #%module-begin)
                     (semicolon \;
                                )
                     (honu-+ +)
                     (honu-* *)
                     (+ scheme:+)
                     (honu-/ /)
                     (honu-- -)
                     (honu-< <)
                     (honu-> >)
                     (honu->= >=)
                     (honu-<= <=)
                     (honu-== ==)
                     (honu-= =)
                     (honu-literal literals)
                     (honu-!= !=)
                     (honu-? ?)
                     (honu-: :)
                     (honu-and and)
                     (honu-comma |,|)
                     (honu-. |.|)
                     (expression-comma expression_comma)
                     )

         (for-syntax (rename-out [syntax-to-string syntax_to_string]))

         #%top

         #%datum
         (for-template #%datum)
         datum->syntax
         #%top-interaction
         (for-syntax #%datum
                     display
                     with-syntax
                     quote
                     #%app
                     #%parens #%brackets #%braces
                     ...
                     map
                     syntax->list
                     expression
                     statement
                     (rename-out (semicolon \;
                                            )
                                 (ellipses-comma ec)
                                 (ellipses-repeat repeat)
                                 (honu-identifier identifier)
                                 (expression-comma expression_comma)
                                 (honu-macro macro)
                                 (parse-an-expr parse)
                                 (... scheme:...)
                                 (honu-body:class body)
                                 (honu-syntax syntax)
                                 (honu-expression-syntax expressionSyntax)
                                 (honu-+ +)
                                 (honu-scheme scheme2)
                                 (scheme-syntax scheme:syntax)
                                 (scheme-syntax schemeSyntax)
                                 ))
         #%braces #%parens #%brackets
         sqrt
         true
         false
         display
         display2
         newline
         with-syntax
         honu-unparsed-begin
         (for-template with-syntax)
         ;; stuff i done want
         define
         let
         ;; end stuff
         else
         lambda
         #%app
         (for-template #%app)
         quote
         ...
         expression
         str
         in-range
         honu-struct
         (rename-out
           (struct scheme-struct)
           (syntax real-syntax)
           (for scheme-for)
           (honu-if if)
           (honu-provide provide)
           (honu-macro-item macroItem)
           (honu-macro macro)
           (honu-infix-macro infixMacro)
           (honu-identifier identifier)
           (honu-identifier identifier123)
           (honu-require require)
           (honu-for-syntax forSyntax)
           (honu-for-template forTemplate)
           (honu-syntax syntax)
           (honu-pattern pattern)
           (honu-keywords keywords)
           (scheme-syntax scheme:syntax)
           ))
