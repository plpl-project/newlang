#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

(define (report-invalid-expression!) (eopl:error 'invalid-expression "this expression is invalid in the current language!"))

(define value-of-program
 (lambda (pgm) (cases prog pgm
    (a-program (scp)
               (val-env->val (value-of (scope-exp scp) (init-env)))))))



; expression, environment -> expval, environment
(define value-of 
 (lambda (expr env) (cases expression expr
    (scope-exp (scp)
      (value-of-exps (scope->exps scp) env))                  
    (var-def-exp (tv) 
      (a-val-env (num-val 1001) env)) ; value can be anything
    (var-def-assign-exp (tp-var exp) 
        (let* ((var (typevar->var tp-var)) (ve (value-of exp env)) (val (val-env->val ve)) (new-env (val-env->env ve))) 
      (a-val-env val (extend-environment var val new-env))))
    (primary-num-exp (num)
      (a-val-env (num-val num) env))
    (primary-bool-exp (bval)
         (let* ((t? (eq? bval 'true)) (val-result (bool-val t?))) 
      (a-val-env val-result env)))
    (primary-string-exp (str)
      (a-val-env (string-val str) env))
                      
    (SUBTRACTION (exp1 exp2)
        (let* ((ve1 (value-of exp1 env)) (val1 (val-env->val ve1)) (new-env1 (val-env->env ve1)) 
               (ve2 (value-of exp2 new-env1)) (val2 (val-env->val ve2)) (new-env2 (val-env->env ve2))
               (num1 (expval->num val1)) (num2 (expval->num val2)) (num-result (- num1 num2))
               (val-result (num-val num-result)))
      (a-val-env val-result new-env2)))
               
     
    (else (report-invalid-expression!))))
  )

(define value-of-exps
  (lambda (es env) (cases exps es
    (empty-exps () (a-val-env (num-val 1002) env)) ; value can be anything
    (nonempty-exps (curexp rest-exps) 
        (let* ((ve (value-of curexp env)) (val (val-env->val ve)) (new-env (val-env->env ve)))
          (if (empty-exps? rest-exps) 
              (a-val-env val new-env) 
              (value-of-exps rest-exps new-env)))))))


;(value-of (primary-string-exp "my string") (init-env))

(provide (all-defined-out))