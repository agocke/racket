#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote make/setup-extension))
(check-docs (quote make/make))
(check-docs (quote make/make-unit))
(check-docs (quote make/make-sig))
(check-docs (quote make))
(check-docs (quote make/collection))
(check-docs (quote make/collection-unit))
(check-docs (quote make/collection-sig))