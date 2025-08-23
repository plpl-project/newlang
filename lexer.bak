#lang racket

(require 
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; Token definitions
(define-empty-tokens op-tokens
  (LPAREN RPAREN
   LBRACE RBRACE
   LBRACK RBRACK
   SEMICOLON COMMA DEFINE ASSIGN
   LT GT EQ LE GE NEQ
   PLUS MINUS TIMES FDIV QDIV REM
   BAND BOR BNEG BLSHIFT BRSHIFT
   AND OR NOT XOR
   IF THEN ELSE WHILE RETURN FUNC NULL
   CONS CAR CDR APPEND LIST
   SUBSTRING STRING-APPEND STRING-LENGTH STRING-SET STRING-AT
   PRINT
   EOF))

(define-tokens value-tokens
  (ID TYPE INT FLOAT BOOL STRING CONST))


(define-lex-abbrevs
  ; Basic components
  [digit        (char-range #\0 #\9)]
  [letter       (:or (char-range #\a #\z) (char-range #\A #\Z))]
  
  ; Identifiers and types
  [type         (:: (:* "list_of ") (:or "int" "bool" "string" "void" "float" "char" "method"))]
  [identifier   (:seq letter (:* (:or letter digit #\_)))]

  
  ; Literals
  [float        (:: (:+ digit) "." (:+ digit))]
  [integer       (:+ digit)]
  
  [boolean      (:or "true" "false")]
  [string-lit   (:seq #\" (:* (:~ #\")) #\")]
  
  ; Whitespace
  [whitespace   (:+ (:or " " "\t" "\n" "\r"))]
)







;; The lexer
(define my-lexer
  (lexer
    [(eof) (token-EOF)]
    [whitespace (my-lexer input-port)]

    ;; Symbols and operators
    ["("  (token-LPAREN)]
    [")"  (token-RPAREN)]
    ["{"  (token-LBRACE)]
    ["}"  (token-RBRACE)]
    ["["  (token-LBRACK)]
    ["]"  (token-RBRACK)]
    [";"  (token-SEMICOLON)]
    [","  (token-COMMA)]
    [":=" (token-DEFINE)]
    ["="  (token-ASSIGN)]
    
    ["<"  (token-LT)]
    [">"  (token-GT)]
    ["==" (token-EQ)]
    ["<=" (token-LE)]
    [">=" (token-GE)]
    ["!=" (token-NEQ)]
    
    ["+"  (token-PLUS)]
    ["-"  (token-MINUS)]
    ["*"  (token-TIMES)]
    ["/"  (token-FDIV)]
    ["//" (token-QDIV)]
    ["%" (token-REM)]
    ["&" (token-BAND)]
    ["|" (token-BOR)]
    ["~" (token-BNEG)]
    ["<<" (token-BLSHIFT)]
    [">>" (token-BRSHIFT)]
    
    ["and" (token-AND)]
    ["or"  (token-OR)]
    ["xor" (token-XOR)]
    ["not" (token-NOT)]
    
    ["if" (token-IF)]
    ["then" (token-THEN)]
    ["else" (token-ELSE)]
    ["while" (token-WHILE)]
    ["return" (token-RETURN)] ; ezafi
    ["func" (token-FUNC)]
    ["boo!" (token-NULL)]
    ["cons" (token-CONS)]
    ["car" (token-CAR)]
    ["cdr" (token-CDR)]
    ["append" (token-APPEND)]
    ["list" (token-LIST)]
    ["substring" (token-SUBSTRING)]
    ["string-append" (token-STRING-APPEND)]
    ["string-length" (token-STRING-LENGTH)]
    ["string-at" (token-STRING-AT)]
    ["string-set" (token-STRING-SET)]
    ["/=\\" (token-PRINT)]
    [(:: #\/ #\*)
     (comment-lexer input-port)]
    

    ;; Literals
    [type  (token-TYPE lexeme)]
    [boolean  (token-BOOL (string->symbol lexeme))]
    [integer (token-INT (string->number lexeme))]
    [float  (token-FLOAT (string->number lexeme))]
    [string-lit    (token-STRING
                    (substring lexeme 1(sub1 (string-length lexeme)))
                    ;lexeme
                    )]
    [identifier (token-ID lexeme)]
  ))

(define comment-lexer 
  (lexer
   [(:: #\* #\/)
    (my-lexer input-port)]
   
   [any-char
    (comment-lexer input-port)]))

(provide (all-defined-out))


(define sample-prog
  "{ 
     x := 1; 
     if(x) then { print(x); } else { x := x + 1; } 
   }")

;(let ([l (open-input-string sample-prog)])
 ;       (my-lexer l)
  ;      )


(define (lex-this lexer input) (lambda () (lexer input)))

