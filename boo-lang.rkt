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
    
    
    (substr-exp (e1 e2 e3)
        (let* ((ve1 (value-of e1 env)) (val1 (val-env->val ve1)) (new-env1 (val-env->env ve1))
               (ve2 (value-of e2 new-env1)) (val2 (val-env->val ve2)) (new-env2 (val-env->env ve2))
               (ve3 (value-of e3 new-env2)) (val3 (val-env->val ve3)) (new-env3 (val-env->env ve3))
               (str (expval->string val1)) (start (expval->num val2)) (len (expval->num val3))
               (substr-result (substring str start (+ start len)))
               (val-result (string-val substr-result)))
      (a-val-env val-result new-env3)))
    (str-append-exp (e1 e2)
        (let* ((ve1 (value-of e1 env)) (val1 (val-env->val ve1)) (new-env1 (val-env->env ve1))
               (ve2 (value-of e2 new-env1)) (val2 (val-env->val ve2)) (new-env2 (val-env->env ve2))
               (str1 (expval->string val1)) (str2 (expval->string val2))
               (str-result (string-append str1 str2))
               (val-result (string-val str-result)))
      (a-val-env val-result new-env2)))

    (str-len-exp (el)
        (let* ((ve (value-of el env)) (val (val-env->val ve)) (new-env (val-env->env ve))
               (str (expval->string val))
               (len-result (string-length str))
               (val-result (num-val len-result)))
      (a-val-env val-result new-env)))
    (STRING-AT-exp (e1 e2)
        (let* ((ve1 (value-of e1 env)) (val1 (val-env->val ve1)) (new-env1 (val-env->env ve1))
               (ve2 (value-of e2 new-env1)) (val2 (val-env->val ve2)) (new-env2 (val-env->env ve2))
               (str (expval->string val1)) (index (expval->num val2))
               (char-result (string-ref str index))
               (val-result (string-val (string char-result))))
      (a-val-env val-result new-env2)))

    (STRING-SET-exp (str-exp index-exp new-char-exp)
        (let* ([str-val (value-of str-exp env)]
               [index-val (value-of index-exp env)]
               [char-val (value-of new-char-exp env)]
               [str (expval->string (val-env->val str-val))]
               [index (expval->num (val-env->val index-val))]
               [new-char (expval->string (val-env->val char-val))])
          (let ([str-list (string->list str)])
            (if (and (>= index 0) (< index (length str-list)))
                (let ([new-str-list (append (take str-list index)
                                          (list (string-ref new-char 0))
                                          (drop str-list (+ index 1)))])
                  (a-val-env (string-val (list->string new-str-list)) env))
                (eopl:error 'STRING-SET "Index out of bounds: ~s" index)))))

    (BOO-EXP ()
        (a-val-env (boo-val) env))

    (PRINT-BOO (left)
    (let* ((ve (value-of left env)) (val (val-env->val ve)) (new-env (val-env->env ve)))
      (begin
        (display "BOO: ")
        (cases expval val
          (boo-val () (display "null"))
          (num-val (n) (display n))
          (bool-val (b) (display b))
          (string-val (s) (display s))
          (list-val (lst) (display lst))
          (else (display val)))
        (newline)
        (a-val-env val new-env))))


    ;; LIST-EXP - create a list from exps structure
    (LIST-EXP (el)
        (eval-exps el env))

    ;; cons-exp - prepend element to list
   (cons-exp (element-exp list-exp)
  (let ([val-env1 (value-of element-exp env)])
    (let ([element-val (val-env->val val-env1)]
          [env1 (val-env->env val-env1)])
      (let ([val-env2 (value-of list-exp env1)])
        (let ([list-val-result (val-env->val val-env2)]
              [env2 (val-env->env val-env2)])
          (let ([existing-list (expval->list list-val-result)])
            (a-val-env (list-val (cons element-val existing-list)) env2)))))))
     ;; append-exp - concatenate two lists
 (append-exp (list1-exp list2-exp)
  (let ([val-env1 (value-of list1-exp env)])
    (let ([list1-val (val-env->val val-env1)]
          [env1 (val-env->env val-env1)])
      (let ([val-env2 (value-of list2-exp env1)])
        (let ([list2-val (val-env->val val-env2)]
              [env2 (val-env->env val-env2)])
          (let ([lst1 (expval->list list1-val)]
                [lst2 (expval->list list2-val)])
            (a-val-env (list-val (append lst1 lst2)) env2)))))))
   ;; car-exp - get first element of list
  (car-exp (list-exp)
    (let ([val-env (value-of list-exp env)])
      (let ([list-val-result (val-env->val val-env)]
            [new-env (val-env->env val-env)])
      (let ([lst (expval->list list-val-result)])
        (if (null? lst)
            (eopl:error 'car-exp "Cannot take car of empty list")
            (a-val-env (car lst) new-env))))))

   ;; cdr-exp - get rest of list
  (cdr-exp (list-exp)
    (let ([val-env (value-of list-exp env)])
       (let ([list-val-result (val-env->val val-env)]
             [new-env (val-env->env val-env)])
       (let ([lst (expval->list list-val-result)])
         (if (null? lst)
             (eopl:error 'cdr-exp "Cannot take cdr of empty list")
             (a-val-env (list-val (cdr lst)) new-env))))))
                      
  (return-arr-val (a-list index-exp)
   (let ([val-env1 (value-of a-list env)])
      (let ([list-val-result (val-env->val val-env1)]
           [env1 (val-env->env val-env1)])
       (let ([val-env2 (value-of index-exp env1)])
         (let ([index-val (val-env->val val-env2)]
               [env2 (val-env->env val-env2)])
          (let ([lst (expval->list list-val-result)]
                [index (expval->num index-val)])
            (if (or (< index 0) (>= index (length lst)))
                (eopl:error 'return-arr-val "Array index out of bounds: ~s" index)
                (a-val-env (list-ref lst index) env2))))))))
                      
   (assign-arr-val (a-list index-exp value-exp)
     (let ([val-env1 (value-of a-list env)])
       (let ([list-val-result (val-env->val val-env1)]
             [env1 (val-env->env val-env1)])
       (let ([val-env2 (value-of index-exp env1)])
         (let ([index-val (val-env->val val-env2)]
               [env2 (val-env->env val-env2)])
          (let ([val-env3 (value-of value-exp env2)])
            (let ([new-val (val-env->val val-env3)]
                  [env3 (val-env->env val-env3)])
              (let ([lst (expval->list list-val-result)]
                    [index (expval->num index-val)])
                (if (or (< index 0) (>= index (length lst)))
                    (eopl:error 'assign-arr-val "Array index out of bounds: ~s" index)
                    (letrec ([update-list (lambda (lst idx val pos)
                                            (if (null? lst)
                                                '()
                                                (if (= pos idx)
                                                    (cons val (cdr lst))
                                                    (cons (car lst) (update-list (cdr lst) idx val (+ pos 1))))))])
                      (a-val-env (list-val (update-list lst index new-val 0)) env3)))))))))))
     
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

;; Helper function to evaluate exps datatype
(define eval-exps
  (lambda (exps-val env)
    (cases exps exps-val
      [empty-exps () (a-val-env (list-val '()) env)]
      [nonempty-exps (curexp rest)
        (let ([val-env1 (value-of curexp env)])
          (let ([cur-val (val-env->val val-env1)]
                [env1 (val-env->env val-env1)])
            (let ([val-env2 (eval-exps rest env1)])
              (let ([rest-list-val (val-env->val val-env2)]
                    [env2 (val-env->env val-env2)])
                (let ([rest-list (expval->list rest-list-val)])
                  (a-val-env (list-val (cons cur-val rest-list)) env2))))))])))



(provide (all-defined-out))