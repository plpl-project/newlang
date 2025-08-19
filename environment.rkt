#lang racket

(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define (report-no-binding-found! var) (eopl:error 'binding-dismatch "\n\tidentifier ~s is used before its declaration!" var))

(define extend-env (lambda (var val env) (extended-environment var val env)))

(define proc-env (lambda (var p-params p-body env) (proc-environment var p-params p-body env)))

(define init-env (lambda () (empty-environment)))

(define apply-env (lambda (var env) (cases environment env
                                      (empty-environment () (report-no-binding-found! var))
                                      (extended-environment (saved-var val saved-env) (if (equal? var saved-var) val (apply-env var saved-env)))
                                      (proc-environment (saved-var p-params  p-body saved-env) 
                                                        (if (equal? var saved-var) (proc-val (a-proc p-params p-body env)) (apply-env var saved-env))))))

(provide (all-defined-out))