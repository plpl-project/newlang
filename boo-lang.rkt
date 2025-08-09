#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

(define (report-invalid-expression!) (eopl:error 'invalid-expression "this expression is invalid in the current language!"))

(define value-of-program
 (lambda (pgm) (cases prog pgm
    (a-program (scp) (value-of (scope-exp scp) (init-env))))))

(define value-of
 (lambda (expr env) (cases expression expr
    
    (SUBTRACTION (exp1 exp2)
        (num-val (- (expval->num (value-of exp1 env)) (expval->num (value-of exp2 env)))))
     
    (else (report-invalid-expression!))))
  )



(provide (all-defined-out))