#lang racket/base

(require racket/contract
         srfi/14/char-set)

(contract-struct-exercise (value-contract char-set-cursor))
(contract-exercise-modules '(srfi/14/char-set))
