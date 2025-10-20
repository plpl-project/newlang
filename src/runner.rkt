#lang racket

(require rackunit)
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")
(require "boo-lang.rkt")



(define path "phase2_questions/q4.txt")
(define (run filename)
    (value-of-program (full-parser (lex-this my-lexer (open-input-file filename)))))
(run path)

