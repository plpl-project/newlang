#lang racket

(require racket/include)

(require 
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "lexer.rkt" "datatypes.rkt")

(define full-parser
  (parser 
   (start scopeexp)
   (end EOF)
   (tokens op-tokens value-tokens)
   (error (lambda (tok-ok? tok-name tok-val)
            (error 'parse "Parse error at ~a: ~a" tok-name tok-val)))
   
   (grammar
    (parexp
     ((LPAREN exp RPAREN) $2))
    (scopeexp
     ((LBRACE RBRACE) (a-scope (empty-exps)))
     ((LBRACE exps RBRACE) (a-scope $2)))
    (exps  ; (exp;)^+
     ((semi-exp) (nonempty-exps $1 (empty-exps)))
     ((semi-exp exps) (nonempty-exps $1 $2)))
    (semi-exp ; exp;
     ((exp SEMICOLON) $1))
    (args
     ((parexp) (nonempty-exps $1 (empty-exps)))
     ((parexp args) (nonempty-exps $1 $2)))
    (type-var
     ((TYPE ID) (a-typevar $1 $2)))
    (params
     ((type-var) (nonempty-params  $1 (empty-params)))
     ((type-var COMMA params) (nonempty-params $1 $3)))
    (func
     ((TYPE FUNC ID LPAREN RPAREN scopeexp) (func-def-exp $1 $3 (empty-params) $6))
     ((TYPE FUNC ID LPAREN params RPAREN scopeexp) (func-def-exp $1 $3 $5 $7)))
    (call
     ((ID LPAREN RPAREN) (func-call-exp $1 (empty-exps)))
     ((ID LPAREN args RPAREN) (func-call-exp $1 $3)))
    (exp
     ((parexp) $1)
     ((scopeexp) (scope-exp $1))
     ((type-var) (var-def-exp $1))
     ((type-var ASSIGN parexp) (var-def-assign-exp $1 $3))
     ((ID ASSIGN parexp) (var-assign-exp $1 $3))
     ((ID) (var-exp $1))
     ((INT) (primary-num-exp $1))
     ((FLOAT) (primary-num-exp $1))
     ((BOOL) (primary-bool-exp $1))
     ((STRING) (primary-string-exp $1))
     ((NULL) (BOO-EXP)) ; DORSA <3
     ((func) $1)
     ((call) $1)
     ((IF parexp THEN scopeexp ELSE scopeexp) (if-then-else-exp $2 $4 $6))
     ((IF parexp THEN scopeexp) (if-then-else-exp $2 $4 (a-scope (empty-exps))))
     ((WHILE parexp scopeexp) (while-exp $2 $3))
     [(CONS parexp  parexp) (cons-exp $2 $3)]
     [(APPEND parexp  parexp) (append-exp $2 $3)]
     [(CAR parexp) (car-exp $2)]
     [(CDR parexp) (cdr-exp $2)]
     [(SUBSTRING parexp  parexp  parexp) (substr-exp $2 $3 $4)]
     [(STRING-APPEND parexp  parexp) (str-append-exp  $2 $3)]
     [(STRING-LENGTH parexp) (str-len-exp $2)]
     [(STRING-AT parexp  parexp) (STRING-AT-exp $2 $3)]
     [(STRING-SET parexp  parexp  parexp) (STRING-SET-exp $2 $3 $4)]

     [(LISTEXP) $1]

     ;; Arithmatic operations
     [(LPAREN exp RPAREN PLUS LPAREN exp RPAREN) (ADDITION $2 $6)]
     [(LPAREN exp RPAREN MINUS LPAREN exp RPAREN) (SUBTRACTION $2 $6)]
     [(LPAREN exp RPAREN TIMES LPAREN exp RPAREN) (MULTIPLY $2 $6)]
     [(LPAREN exp RPAREN REM LPAREN exp RPAREN) (REMAINDER $2 $6)]
     [(LPAREN exp RPAREN FDIV LPAREN exp RPAREN) (FDIVISION $2 $6)]
     [(LPAREN exp RPAREN QDIV LPAREN exp RPAREN) (QDIVISION $2 $6)]

     ;; Comparison operations
     [(LPAREN exp RPAREN LT LPAREN exp RPAREN) (LESSTHAN $2 $6)]
     [(LPAREN exp RPAREN GT LPAREN exp RPAREN) (GREATERTHAN $2 $6)]
     [(LPAREN exp RPAREN EQ LPAREN exp RPAREN) (DOUBLEEQ $2 $6)]
     [(LPAREN exp RPAREN LE LPAREN exp RPAREN) (LESSEQ $2 $6)]
     [(LPAREN exp RPAREN GE LPAREN exp RPAREN) (GREATEREQ $2 $6)]
     [(LPAREN exp RPAREN NEQ LPAREN exp RPAREN) (NOTEQ $2 $6)]

     ;; Binary operations
     [(LPAREN exp RPAREN BAND LPAREN exp RPAREN) (BINAND $2 $6)] 
     [(LPAREN exp RPAREN BOR LPAREN exp RPAREN) (BINOR $2 $6)]     
     [(LPAREN exp RPAREN BLSHIFT LPAREN exp RPAREN) (BINLSHIFT $2 $6)]     
     [(LPAREN exp RPAREN BRSHIFT LPAREN exp RPAREN) (BINRSHIFT $2 $6)]

     ;; Logical operations
     [(LPAREN exp RPAREN AND LPAREN exp RPAREN) (ANDOP $2 $6)]     
     [(LPAREN exp RPAREN OR LPAREN exp RPAREN) (OROP $2 $6)]     
     [(LPAREN exp RPAREN XOR LPAREN exp RPAREN) (XOROP $2 $6)]
     [(NOT LPAREN exp RPAREN) (NOTNOT $3)]
     [(BNEG LPAREN exp RPAREN) (BNEGATION $3)]

     ;; Array Implememntation
     [(exp LBRACK exp RBRACK) (return-arr-val $1 $3)]
     [(exp LBRACK exp RBRACK ASSIGN parexp) (assign-arr-val $1 $3 $6)]

     ;; Print
     [(PRINT parexp) (PRINT-BOO $2)])

    (LISTEXP
     ((LIST LPAREN RPAREN) (LIST-EXP (empty-exps)))
     ((LIST LPAREN args RPAREN) (LIST-EXP $3))
     )
   )))
     

(provide (all-defined-out))
