#lang racket

(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define (report-no-binding-found! var) (eopl:error 'binding-dismatch "\n\tidentifier ~s is used before its declaration!" var))

(define extend-env (lambda (var val env) (extended-environment var val env)))


(define init-env (lambda () (empty-environment)))

(define apply-env (lambda (var env) (cases environment env
                                      (empty-environment () (report-no-binding-found! var))
                                      (extended-environment (saved-var val saved-env) (if (equal? var saved-var) val (apply-env var saved-env))))))
(provide (all-defined-out))