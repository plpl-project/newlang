#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

(define (report-invalid-expression!) (eopl:error 'invalid-expression "this expression is invalid in the current language!"))
(define (report-too-few-args-error!) (eopl:error 'invalid-arity "Too few arguments provided."))
(define (report-too-many-args-error!) (eopl:error 'invalid-arity "Too many arguments provided."))




(define value-of-program
 (lambda (pgm) (cases prog pgm
    (a-program (scp)
               (val-env->val (value-of (scope-exp scp) (init-env)))))))
              ;  (value-of (scope-exp scp) (init-env))))))



; expression, environment -> expval, environment
(define value-of 
 (lambda (expr env) (cases expression expr
    (scope-exp (scp)
      (value-of-exps (scope->exps scp) env))                  
    (var-def-exp (tv) 
      (a-val-env (num-val 1001) env)) ; value can be anything
    (var-def-assign-exp (tp-var exp) 
        (let* ((var (typevar->var tp-var)) (ve (value-of exp env)) (val (val-env->val ve)) (new-env (val-env->env ve))) 
      (a-val-env val (extend-env var val new-env))))  
    (primary-num-exp (num)
      (a-val-env (num-val num) env))
    (primary-bool-exp (bval)
         (let* ((t? (eq? bval 'true)) (val-result (bool-val t?))) 
      (a-val-env val-result env)))
    (primary-string-exp (str)
      (a-val-env (string-val str) env))
    (func-def-exp (type func-name params body-scp) 
        (let* ((param-list (params->list-of-strings params)) 
              (prc (a-proc param-list body-scp env)) (p-val (proc-val prc)) (new-env (extend-env func-name p-val env))) 
      (a-val-env p-val new-env)))
    (func-call-exp (func-name args) 
        (let* ((p-val (apply-env func-name env)) (vse (value-of-args args env)) 
              (new-env (vals-env->env vse)) (vals (vals-env->vals vse)) 
              (result-val (apply-procedure (expval->proc p-val) vals))) 
      (a-val-env result-val new-env)))
                      
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

(define value-of-args
  (lambda (es env) (cases exps es
    (empty-exps () (a-vals-env '() env)) 
    (nonempty-exps (curexp rest-exps) 
        (let* ((ve (value-of curexp env)) (val (val-env->val ve)) (new-env (val-env->env ve))
                (vse (value-of-args rest-exps new-env)) (rest-vals (vals-env->vals vse)) (last-env (vals-env->env vse)))
          (a-vals-env (cons val rest-vals) last-env))))))

(define apply-procedure
 (lambda (pr vals) (cases proc pr
    (a-proc (param-list scp-body saved-env) 
        (let* ((new-env (extend-all param-list vals saved-env)) 
              (ve-result (value-of-exps (scope->exps scp-body) new-env)))
    (val-env->val ve-result))))))

(define extend-all  
  (lambda (vars vals env)(cond 
    ((empty? vars) (if (empty? vals) env (report-too-many-args-error!)))
    ((empty? vals) (report-too-few-args-error!))
    (else (let ((cur-var (car vars)) (rest-vars (cdr vars)) (cur-val (car vals)) (rest-vals (cdr vals))) 
      (extend-all rest-vars rest-vals (extend-env cur-var cur-val env)))))))


(provide (all-defined-out))