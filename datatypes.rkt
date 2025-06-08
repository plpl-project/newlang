#lang racket
(require eopl)
(require (lib "eopl.ss" "eopl"))

(define-datatype typevar typevar?
  (a-typevar (type string?) (var string?)))

(define-datatype params params?
  (empty-params)
  (nonempty-params (ne-param typevar?) (rest params?)))

(define-datatype scope scope?
  (a-scope (exps exps?)))

(define-datatype exps exps?
  (empty-exps)
  (nonempty-exps (curexp exp?) (rest exps?)))

(define-datatype exp exp?
  (scope-exp (scp scope?))
  (var-def-exp (val typevar?))
  (var-def-assign-exp (var typevar?) (value exp?))
  (var-assign-exp (var-name string?) (value exp?))
  (var-exp (var-name string?))
  (primary-num-exp (val number?))
  (primary-bool-exp (val symbol?))
  (primary-string-exp (val string?))
  (func-def-exp (type string?) (func-name string?) (params params?) (scope scope?))
  (func-call-exp (func-name string?) (args exps?))
  (if-then-else-exp (cond exp?) (then-st scope?) (else-st scope?))
  (while-exp (cond exp?) (body scope?))
  (cons-exp (e1 exp?) (e2 exp?))
  (append-exp (e1 exp?) (e2 exp?))
  (car-exp (e exp?))
  (cdr-exp (e exp?))
  (substr-exp (e1 exp?) (e2 exp?) (e3 exp?))
  (str-append-exp (e1 exp?) (e2 exp?))
  (str-len-exp (el exp?))
  (STRING-AT-exp (e1 exp?) (e2 exp?))
  (STRING-SET-exp (e1 exp?) (e2 exp?) (e3 exp?))
  (LIST-EXP (el exps?))
  (BOO-EXP)
  (PRINT-BOO (left exp?))
  (LESSTHAN (left exp?) (right exp?))
  (GREATERTHAN (left exp?) (right exp?))
  (DOUBLEEQ (left exp?) (right exp?))
  (LESSEQ (left exp?) (right exp?))
  (GREATEREQ (left exp?) (right exp?))
  (NOTEQ (left exp?) (right exp?))  
  (FDIVISION (left exp?) (right exp?))
  (QDIVISION (left exp?) (right exp?))
  (ADDITION (left exp?) (right exp?))
  (SUBTRACTION (left exp?) (right exp?))
  (MULTIPLY (left exp?) (right exp?))
  (REMAINDER (left exp?) (right exp?))
  (BINAND (left exp?) (right exp?))
  (BINOR (left exp?) (right exp?))
  (BINLSHIFT (left exp?) (right exp?))
  (BINRSHIFT (left exp?) (right exp?))
  (BNEGATION (left exp?))
  (NOTNOT (left exp?))
  (ANDOP (left exp?) (right exp?))
  (OROP (left exp?) (right exp?))
  (XOROP (left exp?) (right exp?))
  (return-arr-val (a-list exp?) (index exp?))
  (assign-arr-val (a-list exp?) (index exp?) (value exp?)))
  ;() CONST

(define-datatype LISTEXP LISTEXP?
  (a-LISTEXP (args exps?))
)

(provide (all-defined-out))