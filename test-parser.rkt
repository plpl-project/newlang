#lang racket

(require rackunit)
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")
(require "boo-lang.rkt")






(define program
      "
       { 
       x = ((list ((a) (3) (9.235))));
       x[98][9][0] = (2);
       {
       a;
       int x = (0);
       if (false) then {
           if ((3) xor (2))
           then
           {/=\\(\"kal\");};
       } else {int y = ((9));};
       };

       char ch = (\"p\");
       
       {a;b;c;};

       list_of list_of int func f (int a, string b, bool e)
       {
           int a = (list((3)(3)(4)));
           a[not(4)];
       };

       while ((string-append (\"a\") (\" bc\")) != (\"a bc\")){
          string-length (\"email: \");
       };
       
       }")

(define simple-program
      "
   { 
       float x = (0);
       int func f(int a, string b, bool e){
          if ((e)) then {
              (a);
          } else {
              f(((a) - (10)) (b) (true));
          };
       };
       (42) - (542);
       f((4) (\"salamb\") (false));
       x = (1);
       f((x) (\" \") (false));
       bool c = (true);
       while(c){
           int x = ((x) - (12));
           /=\\(x);
           c = (false);
       };
       /=\\(x);

     
   }       
")

(define input (open-input-string simple-program))


;(define p (open-input-string program))

(define AST (full-parser (lex-this my-lexer input)))

(value-of-program AST)


