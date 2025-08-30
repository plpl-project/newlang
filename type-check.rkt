#lang racket


(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl")) 


(struct a-type-env (type env) #:transparent)

(define (init-type-env) '())

(define (extend-type-env var typ type-env)
  (cons (cons var typ) type-env))

(define (apply-type-env var type-env)
  (let ((b (assoc var type-env)))
    (if b (cdr b)
        (eopl:error 'type-error "Unbound variable in type environment: ~s" var))))


(define (report-type-error! . args)
  (apply eopl:error 'type-error args))

; (define (type->string t)
;   (cond [(and (pair? t) (eq? (car t) 'list))
;          (string-append "list_of " (type->string (cadr t)))]
;         [(pair? t) (format "~a" t)]
;         [(or (equal? t "int") (equal? t "float")) "num" ]
;         [else (format "~a" t)]))


(define (list-of-prefix? s)
  (and (string? s)
       (<= 8 (string-length s))
       (string=? (substring s 0 8) "list_of ")))

(define (string->type t)
  (cond
    [(list-of-prefix? t)
     (list 'list (string->type (substring t 8)))]
    [(or (equal? t "int") (equal? t "float")) "num"]
    [else t]))

(define (type-list? t)
  (let ((nt (string->type t)))
    (and (pair? nt) (eq? (car nt) 'list))))
(define (list-elem-type t)
  (let ((nt (string->type t)))
    (if (and (pair? nt) (eq? (car nt) 'list))
        (cadr nt)
        (report-type-error! "Expected list type, got ~s" t))))



(define (type-equal? t1 t2)
  (let ((t1 (string->type t1)) (t2 (string->type t2)))
    (cond 
      ((and (type-list? t1) (type-list? t2)) 
        (or (equal? (list-elem-type t1) "void") (equal? (list-elem-type t2) "void") (equal? t1 t2)))
    (else (equal? t1 t2)))))




(define (params->list-of-types prms)
  (cases params prms
    (empty-params () '())
    (nonempty-params (ne-param rest)
      (cons (string->type (typevar->type ne-param))
            (params->list-of-types rest)))))



(define (type-of expr tenv)
  (cases expression expr
    (scope-exp (scp)
      (type-of-exps (scope->exps scp) tenv))


    (var-def-exp (tv)
        (let* ((var (typevar->var tv))
            (decl-typ (string->type (typevar->type tv))))
    (a-type-env decl-typ (extend-type-env var decl-typ tenv))))


    (var-def-assign-exp (tv exp)
      (let* ((var (typevar->var tv))
             (decl-typ (typevar->type tv))
             (res (type-of exp tenv))
             (t (a-type-env-type res))
             (env2 (a-type-env-env res)))
        (if (type-equal? t decl-typ)
            (a-type-env decl-typ (extend-type-env var decl-typ env2))
            (report-type-error! "Type mismatch: declared ~s but initializer has type ~s" decl-typ t))))

    (var-assign-exp (var-name exp)
      (let* ((res (type-of exp tenv))
             (t (a-type-env-type res))
             (env2 (a-type-env-env res))
             (var-typ (apply-type-env var-name env2)))
        (if (type-equal? t var-typ)
            (a-type-env var-typ env2)
            (report-type-error! "Assignment type mismatch for ~s: expected ~s got ~s" var-name var-typ t))))

    (var-exp (var-name)
      (let ((t (apply-type-env var-name tenv)))
        (a-type-env t tenv)))

    (primary-num-exp (num) (a-type-env "num" tenv))
    (primary-bool-exp (bval) (a-type-env "bool" tenv))
    (primary-string-exp (str) (a-type-env "string" tenv))

    (func-def-exp (ret-type func-name params body-scp)
      (let* ((p-names (params->list-of-strings params))
             (p-types (params->list-of-types params)))
        (if (not (= (length p-names) (length p-types)))
            (report-type-error! "Function ~s: parameter names/types count mismatch" func-name)
            (let* ((proc-typ (list 'proc p-types ret-type))
                (tenv1 (extend-type-env func-name proc-typ tenv))
                (tenv2 (foldl (lambda (pair acc) (extend-type-env (car pair) (cdr pair) acc))
                                tenv1
                                (map cons p-names p-types)))
                (res (type-of-exps (scope->exps body-scp) tenv2))
                (body-typ (a-type-env-type res))
                (env-after (a-type-env-env res)))
            (if (type-equal? body-typ ret-type)
                (a-type-env proc-typ tenv1)
                (report-type-error! "Function ~s declared return type ~s but body has type ~s" func-name ret-type body-typ))))))

    (func-call-exp (func-name args)
      (let ((f-typ (apply-type-env func-name tenv)))
        (if (and (pair? f-typ) (eq? (car f-typ) 'proc))
            (let* ((param-types (cadr f-typ))
                    (ret-type (caddr f-typ))
                    (arg-res (type-of-args args tenv))
                    (arg-types (a-tys-args-types arg-res))
                    (env-after (a-tys-args-env arg-res)))
                (when (not (= (length param-types) (length arg-types)))
                    (report-type-error! "Arity mismatch calling ~s: expected ~s args, got ~s" func-name (length param-types) (length arg-types)))
                (for-each (lambda (pt at)
                            (unless (type-equal? pt at)
                                (report-type-error! "Argument type mismatch calling ~s: expected ~s got ~s" func-name pt at)))
                            param-types arg-types)
                (a-type-env ret-type env-after))
          (report-type-error! "Attempt to call non-procedure ~s (type ~s)" func-name f-typ))))

    (if-then-else-exp (condition then-sp else-sp)
      (let* ((cond-res (type-of condition tenv))
             (cond-typ (a-type-env-type cond-res))
             (env1 (a-type-env-env cond-res)))
            (if (type-equal? cond-typ "bool")
                (let* ((then-res (type-of-exps (scope->exps then-sp) env1))
                        (then-typ (a-type-env-type then-res))
                        (env2 (a-type-env-env then-res))
                        (else-res (type-of-exps (scope->exps else-sp) env2))
                        (else-typ (a-type-env-type else-res))
                        (env3 (a-type-env-env else-res)))
                    (if (type-equal? then-typ else-typ)
                        (a-type-env then-typ env3)
                        (report-type-error! "If branches must have same type: then ~s else ~s" then-typ else-typ)))
                (report-type-error! "If condition must be bool, got ~s" cond-typ))))


    (while-exp (condition body-scp)
      (let* ((cond-res (type-of condition tenv))
             (cond-typ (a-type-env-type cond-res))
             (env1 (a-type-env-env cond-res)))
        (unless (type-equal? cond-typ "bool")
          (report-type-error! "While condition must be bool, got ~s" cond-typ))
        (let ((body-res (type-of-exps (scope->exps body-scp) env1)))
          (a-type-env "void" (a-type-env-env body-res)))))

    (PRINT-BOO (e)
      (let ((res (type-of e tenv)))
        (a-type-env (a-type-env-type res) (a-type-env-env res))))

    ;; strings
    (substr-exp (e1 e2 e3)
      (let* ((r1 (type-of e1 tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
             (r2 (type-of e2 env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2))
             (r3 (type-of e3 env2)) (t3 (a-type-env-type r3)) (env3 (a-type-env-env r3)))
        (unless (type-equal? t1 "string") (report-type-error! "substring expects string, got ~s" t1))
        (unless (type-equal? t2 "num") (report-type-error! "substring start must be num, got ~s" t2))
        (unless (type-equal? t3 "num") (report-type-error! "substring length must be num, got ~s" t3))
        (a-type-env "string" env3)))

    (str-append-exp (e1 e2)
      (let* ((r1 (type-of e1 tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
             (r2 (type-of e2 env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2)))
        (unless (type-equal? t1 "string") (report-type-error! "str-append expects string, got ~s" t1))
        (unless (type-equal? t2 "string") (report-type-error! "str-append expects string, got ~s" t2))
        (a-type-env "string" env2)))

    (str-len-exp (el)
      (let* ((r (type-of el tenv)) (t (a-type-env-type r)) (env2 (a-type-env-env r)))
        (unless (type-equal? t "string") (report-type-error! "str-len expects string, got ~s" t))
        (a-type-env "num" env2)))

    (STRING-AT-exp (e1 e2)
      (let* ((r1 (type-of e1 tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
             (r2 (type-of e2 env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2)))
        (unless (type-equal? t1 "string") (report-type-error! "string-at expects string, got ~s" t1))
        (unless (type-equal? t2 "num") (report-type-error! "string-at expects numeric index, got ~s" t2))
        (a-type-env "string" env2)))

    (STRING-SET-exp (str-exp index-exp new-char-exp)
      (let* ((r1 (type-of str-exp tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
             (r2 (type-of index-exp env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2))
             (r3 (type-of new-char-exp env2)) (t3 (a-type-env-type r3)) (env3 (a-type-env-env r3)))
        (unless (type-equal? t1 "string") (report-type-error! "STRING-SET expects string, got ~s" t1))
        (unless (type-equal? t2 "num") (report-type-error! "STRING-SET expects numeric index, got ~s" t2))
        (unless (type-equal? t3 "string") (report-type-error! "STRING-SET expects new char as string, got ~s" t3))
        (a-type-env "string" env3)))

    (BOO-EXP () (a-type-env "void" tenv))

    (LIST-EXP (el)
      (type-of-list-literal el tenv))


    (cons-exp (element-exp list-exp)
        (let* ((r1 (type-of element-exp tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
             (r2 (type-of list-exp env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2)))
        (unless (type-list? t2)
        (report-type-error! "cons: second argument must be list, got ~s" t2))
        (let ((elem-typ (list-elem-type t2)))
            (unless (type-equal? t1 elem-typ)
            (report-type-error!
                "cons: element type ~s doesn't match list element type ~s" t1 elem-typ))
      (a-type-env (string->type t2) env2))))

    (append-exp (list1-exp list2-exp)
        (let* ((r1 (type-of list1-exp tenv))
            (t1 (a-type-env-type r1))
            (env1 (a-type-env-env r1))
            (r2 (type-of list2-exp env1))
            (t2 (a-type-env-type r2))
            (env2 (a-type-env-env r2)))
        (unless (type-list? t1)
            (report-type-error! "append expects list, got ~s" t1))
        (unless (type-list? t2)
            (report-type-error! "append expects list, got ~s" t2))
        (unless (type-equal? (list-elem-type t1) (list-elem-type t2))
            (report-type-error! "append: list element types differ: ~s vs ~s" (list-elem-type t1) (list-elem-type t2)))
        (a-type-env (string->type t1) env2)))


    (car-exp (list-exp)
        (let* ((r (type-of list-exp tenv))
            (t (a-type-env-type r))
            (env2 (a-type-env-env r)))
        (unless (type-list? t)
        (report-type-error! "car expects list, got ~s" t))
        (a-type-env (list-elem-type t) env2)))
    
    (cdr-exp (list-exp)
        (let* ((r (type-of list-exp tenv))
            (t (a-type-env-type r))
            (env2 (a-type-env-env r)))
        (unless (type-list? t)
        (report-type-error! "cdr expects list, got ~s" t))
        (a-type-env (string->type t) env2)))

    (len-exp (list-exp)
        (let* ((r (type-of list-exp tenv))
            (t (a-type-env-type r))
            (env2 (a-type-env-env r)))
        (unless (type-list? t)
        (report-type-error! "how-long expects list, got ~s" t))
        (a-type-env "num" env2)))

    (empty-exp (list-exp)
        (let* ((r (type-of list-exp tenv))
            (t (a-type-env-type r))
            (env2 (a-type-env-env r)))
        (unless (type-list? t)
        (report-type-error! "is-empty expects list, got ~s" t))
        (a-type-env "bool" env2)))

    


    (return-arr-val (a-list index-exp)
      (let* ((r1 (type-of a-list tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
             (r2 (type-of index-exp env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2)))
        (unless (type-list? t1) (report-type-error! "indexing expects list/array, got ~s" t1))
        (unless (type-equal? t2 "num") (report-type-error! "array index must be num, got ~s" t2))
        (a-type-env (list-elem-type t1) env2)))

    (assign-arr-val (a-list index-exp value-exp)
      (let* ((r1 (type-of a-list tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
             (r2 (type-of index-exp env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2))
             (r3 (type-of value-exp env2)) (t3 (a-type-env-type r3)) (env3 (a-type-env-env r3)))
        (unless (type-list? t1) (report-type-error! "array assign expects list/array, got ~s" t1))
        (unless (type-equal? t2 "num") (report-type-error! "array index must be num, got ~s" t2))
        (unless (type-equal? (list-elem-type t1) t3) (report-type-error! "assign-arr: value type ~s doesn't match array element type ~s" t3 (cadr t1)))
        (a-type-env t1 env3)))

    (LESSTHAN (exp1 exp2)
      (numeric-compare-type " < " exp1 exp2 tenv))
    (GREATERTHAN (exp1 exp2)
      (numeric-compare-type " > " exp1 exp2 tenv))
    (DOUBLEEQ (exp1 exp2)
      (equality-compare-type exp1 exp2 tenv))
    (GREATEREQ (exp1 exp2)
      (numeric-compare-type " >= " exp1 exp2 tenv))
    (LESSEQ (exp1 exp2)
      (numeric-compare-type " <= " exp1 exp2 tenv))
    (NOTEQ (exp1 exp2)
      (equality-compare-type exp1 exp2 tenv))

    (FDIVISION (exp1 exp2)
      (numeric-binary-type " / " exp1 exp2 tenv))
    (QDIVISION (exp1 exp2)
      (numeric-binary-type " % " exp1 exp2 tenv))
    (ADDITION (exp1 exp2)
      (numeric-binary-type " + " exp1 exp2 tenv))
    (SUBTRACTION (exp1 exp2)
      (numeric-binary-type " - " exp1 exp2 tenv))
    (MULTIPLY (exp1 exp2)
      (numeric-binary-type " * " exp1 exp2 tenv))
    (REMAINDER (exp1 exp2)
      (numeric-binary-type " rem " exp1 exp2 tenv))

    (BINAND (exp1 exp2) (numeric-binary-type " & " exp1 exp2 tenv))
    (BINOR  (exp1 exp2) (numeric-binary-type " | " exp1 exp2 tenv))
    (BINLSHIFT (exp1 exp2) (numeric-binary-type " << " exp1 exp2 tenv))
    (BINRSHIFT (exp1 exp2) (numeric-binary-type " >> " exp1 exp2 tenv))
    (BNEGATION (exp1) (numeric-unary-type "~" exp1 tenv))
    (NEGATION (exp1) (numeric-unary-type "-" exp1 tenv))

    (NOTNOT (exp1)
      (let* ((r (type-of exp1 tenv)) (t (a-type-env-type r)) (env1 (a-type-env-env r)))
        (unless (type-equal? t "bool") (report-type-error! "not expects bool, got ~s" t))
        (a-type-env "bool" env1)))

    (ANDOP (exp1 exp2)
      (logical-binop-type "and" exp1 exp2 tenv))
    (OROP  (exp1 exp2)
      (logical-binop-type "or" exp1 exp2 tenv))
    (XOROP (exp1 exp2)
      (logical-binop-type "xor" exp1 exp2 tenv))

    (else (report-type-error! "Unknown expression in type checker: ~s" expr))))


(define (type-of-exps es tenv)
  (cases exps es
    (empty-exps () (a-type-env "void" tenv))
    (nonempty-exps (curexp rest-exps)
      (let ((res (type-of curexp tenv)))
        (let ((t (a-type-env-type res)) (env1 (a-type-env-env res)))
          (if (empty-exps? rest-exps)
              (a-type-env t env1)
              (type-of-exps rest-exps env1)))))))

(struct a-tys-args (types env) #:transparent)

(define (type-of-args es tenv)
  (cases exps es
    (empty-exps () (a-tys-args '() tenv))
    (nonempty-exps (curexp rest-exps)
      (let* ((r (type-of curexp tenv))
             (t (a-type-env-type r))
             (env1 (a-type-env-env r))
             (rest (type-of-args rest-exps env1)))
        (a-tys-args (cons t (a-tys-args-types rest)) (a-tys-args-env rest))))))

(define (type-of-list-literal exps-list tenv)
  (cases exps exps-list
    (empty-exps () (a-type-env (list 'list "void") tenv))
    (nonempty-exps (curexp rest)
      (let* ((r (type-of curexp tenv))
             (t (a-type-env-type r))
             (env1 (a-type-env-env r)))
        (if (empty-exps? rest)
            (a-type-env (list 'list t) env1)
            (let ((rest-res (type-of-list-literal rest env1)))
              (let ((rest-typ (a-type-env-type rest-res))
                    (env2 (a-type-env-env rest-res)))
                (unless (type-equal? (cadr rest-typ) t)
                  (report-type-error! "List literal: element types differ: ~s vs ~s" t (cadr rest-typ)))
                (a-type-env rest-typ env2))))))))


(define (numeric-binary-type opname e1 e2 tenv)
  (let* ((r1 (type-of e1 tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
         (r2 (type-of e2 env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2)))
    (unless (type-equal? t1 "num") (report-type-error! "~a expects num, left was ~s" opname t1))
    (unless (type-equal? t2 "num") (report-type-error! "~a expects num, right was ~s" opname t2))
    (a-type-env "num" env2)))

(define (numeric-unary-type opname e type-env)
  (let* ((r (type-of e type-env)) (t (a-type-env-type r)) (env1 (a-type-env-env r)))
    (unless (type-equal? t "num") (report-type-error! "~a expects num, got ~s" opname t))
    (a-type-env "num" env1)))

(define (numeric-compare-type opname e1 e2 tenv)
  (let* ((res (numeric-binary-type opname e1 e2 tenv)))
    (a-type-env "bool" (a-type-env-env res))))

(define (equality-compare-type e1 e2 tenv)
    (let* ((r1 (type-of e1 tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1))
         (r2 (type-of e2 env1)) (t2 (a-type-env-type r2)) (env2 (a-type-env-env r2)))
    (a-type-env "bool" env2)))

(define (logical-binop-type opname e1 e2 tenv)
  (let* ((r1 (type-of e1 tenv)) (t1 (a-type-env-type r1)) (env1 (a-type-env-env r1)))
    (unless (type-equal? t1 "bool") (report-type-error! "~a expects bool on left, got ~s" opname t1))
    (let ((r2 (type-of e2 env1)))
      (unless (type-equal? (a-type-env-type r2) "bool") (report-type-error! "~a expects bool on right, got ~s" opname (a-type-env-type r2)))
      (a-type-env "bool" (a-type-env-env r2)))))



(provide (all-defined-out))