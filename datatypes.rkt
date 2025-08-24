#lang racket
(require eopl)
(require (lib "eopl.ss" "eopl"))


; error handling
(define (report-expval-extractor-error! type) (eopl:error 'invalid-value "invalid value cast to ~s" type))
(define (report-extractor-error! type) (eopl:error 'invalid-value "invalid value to extract from ~s" type))


; AST types

(define-datatype typevar typevar?
  (a-typevar (type string?) (var string?)))

(define-datatype params params?
  (empty-params)
  (nonempty-params (ne-param typevar?) (rest params?)))

(define-datatype prog prog?
  (a-program (scope scope?)))

(define-datatype scope scope?
  (a-scope (exps exps?)))

(define-datatype exps exps?
  (empty-exps)
  (nonempty-exps (curexp expression?) (rest exps?)))

(define-datatype expression expression?
  (scope-exp (scp scope?))
  (var-def-exp (val typevar?))
  (var-def-assign-exp (var typevar?) (value expression?))
  (var-assign-exp (var-name string?) (value expression?))
  (var-exp (var-name string?))
  (primary-num-exp (val number?))
  (primary-bool-exp (val symbol?))
  (primary-string-exp (val string?))
  (func-def-exp (type string?) (func-name string?) (params params?) (body-scp scope?))
  (func-call-exp (func-name string?) (args exps?))
  (if-then-else-exp (cond expression?) (then-st scope?) (else-st scope?))
  (while-exp (cond expression?) (body scope?))
  (cons-exp (e1 expression?) (e2 expression?))
  (append-exp (e1 expression?) (e2 expression?))
  (car-exp (e expression?))
  (cdr-exp (e expression?))
  (substr-exp (e1 expression?) (e2 expression?) (e3 expression?))
  (str-append-exp (e1 expression?) (e2 expression?)) 
  (str-len-exp (el expression?)) 
  (STRING-AT-exp (e1 expression?) (e2 expression?))
  (STRING-SET-exp (e1 expression?) (e2 expression?) (e3 expression?))
  ;(LIST-EXP (el (list-of expression?)))  ; list of expressions
  (LIST-EXP (el exps?)) 
  (BOO-EXP)
  (PRINT-BOO (left expression?))
  (LESSTHAN (left expression?) (right expression?))
  (GREATERTHAN (left expression?) (right expression?))
  (DOUBLEEQ (left expression?) (right expression?))
  (LESSEQ (left expression?) (right expression?))
  (GREATEREQ (left expression?) (right expression?))
  (NOTEQ (left expression?) (right expression?))  
  (FDIVISION (left expression?) (right expression?))
  (QDIVISION (left expression?) (right expression?))
  (ADDITION (left expression?) (right expression?))
  (SUBTRACTION (left expression?) (right expression?))
  (MULTIPLY (left expression?) (right expression?))
  (REMAINDER (left expression?) (right expression?))
  (BINAND (left expression?) (right expression?))
  (BINOR (left expression?) (right expression?))
  (BINLSHIFT (left expression?) (right expression?))
  (BINRSHIFT (left expression?) (right expression?))
  (BNEGATION (left expression?))
  (NOTNOT (left expression?))
  (ANDOP (left expression?) (right expression?))
  (OROP (left expression?) (right expression?))
  (XOROP (left expression?) (right expression?))
  (return-arr-val (a-list expression?) (index expression?)) 
  (assign-arr-val (a-list expression?) (index expression?) (value expression?))
  (len-exp (e expression?)) 
  (empty-exp (e expression?))
  (NEGATION (e expression?)))
  ;() CONST

(define-datatype LISTEXP LISTEXP?
  (a-LISTEXP (args exps?))
)



; additional types

(define-datatype environment environment?
  (empty-environment)
  (extended-environment (saved-var string?)
                      (val integer?) ; index
                      (saved-env environment?))
)
 

(define-datatype proc proc?
  (a-proc (params (list-of string?)) ; not sure
             (body scope?)
             (saved-env environment?)))

(define-datatype val-env val-env?
  (a-val-env (value expval?)
             (env environment?))
  )

(define-datatype vals-env vals-env?
  (a-vals-env (values (list-of expval?))
             (env environment?))
  )


; expression values 

(define-datatype expval expval?
 (num-val (num number?))
 (bool-val (bool boolean?))
 (string-val (str string?))
 (proc-val (prc proc?))
 (ref-val (int integer?))
 (list-val (lst (list-of expval?)))
 (boo-val))

(define (same-type? exp1 exp2)
  (cases expval exp1
    (num-val (num) (cases expval exp2 (num-val (num) #t) (else #f)))
    (bool-val (bool) (cases expval exp2 (bool-val (bool) #t) (else #f)))
    (string-val (str) (cases expval exp2 (string-val (str) #t) (else #f)))
    (proc-val (prc) (cases expval exp2 (proc-val (prc) #t) (else #f)))
    (ref-val (int) (cases expval exp2 (ref-val (int) #t) (else #f)))
    (list-val (lst) (cases expval exp2 (list-val (lst) #t) (else #f)))
    (boo-val () (cases expval exp2 (boo-val () #t) (else #f)))
  )
)


 ;expvals to their values

 (define expval->num
 (lambda (val) (cases expval val
    (num-val (num) num)
    (else (report-expval-extractor-error! "number")))))

(define expval->bool
 (lambda (val) (cases expval val
    (bool-val (bool) bool)
    (else (report-expval-extractor-error! "boolean")))))

(define expval->string
 (lambda (val) (cases expval val
    (string-val (str) str)
    (else (report-expval-extractor-error! "string")))))

(define expval->proc
 (lambda (val) (cases expval val
    (proc-val (proc) proc)
    (else (report-expval-extractor-error! "proc")))))

(define expval->ref
 (lambda (val) (cases expval val
    (ref-val (int) int)
    (else (report-expval-extractor-error! "reference")))))


(define expval->list
 (lambda (val) (cases expval val
    (list-val (lst) lst)
    (else (report-expval-extractor-error! "list")))))

(define expval->boo
 (lambda (val) (cases expval val
    (boo-val () 'boo)
    (else (report-expval-extractor-error! "boo")))))

(define expval->printable
 (lambda (val) (cases expval val
    (num-val (num) num)
    (bool-val (bool) bool)
    (string-val (str) str)
    (proc-val (proc) proc)
    (ref-val (int) int)
    (list-val (lst) lst) 
    (boo-val () "null")
    (else val))))

;extractor

(define typevar->var 
  (lambda (tv) (cases typevar tv
    (a-typevar (type var) var)
    (else (report-extractor-error! "typevar")))))

(define typevar->type 
  (lambda (tv) (cases typevar tv
    (a-typevar (type var) type)
    (else (report-extractor-error! "typevar")))))

(define val-env->val 
  (lambda (input) (cases val-env input
    (a-val-env (val env) val)
    (else (report-extractor-error! "val-env")))))

(define val-env->env 
  (lambda (input) (cases val-env input
    (a-val-env (val env) env)
    (else (report-extractor-error! "val-env")))))

(define vals-env->vals 
  (lambda (input) (cases vals-env input
    (a-vals-env (vals env) vals)
    (else (report-extractor-error! "vals-env")))))

(define vals-env->env 
  (lambda (input) (cases vals-env input
    (a-vals-env (val env) env)
    (else (report-extractor-error! "vals-env")))))

(define scope->exps
  (lambda (scp) (cases scope scp
    (a-scope (es) es)
    (else (report-extractor-error! "scope")))))


; helpers
(define empty-exps? 
  (lambda (input) (cases exps input
  (empty-exps () #t)
  (else #f))))

;convertor 

  ; from params to list of variable names
(define params->list-of-strings
  (lambda (prms) (cases params prms
  (empty-params () '())
  (nonempty-params (ne-param rest) (cons (typevar->var ne-param) (params->list-of-strings rest)))))) 



(provide (all-defined-out))