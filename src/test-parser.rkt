#lang racket

(require rackunit)
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")
(require "boo-lang.rkt")



(define path "examples/q5.txt")
(define (run filename)
    (value-of-program (full-parser (lex-this my-lexer (open-input-file filename)))))
(run path)

#|
(define program
      "
       { 
       x = ((list ((a) (3) (9.235))));
       x[98][9][0] = (2);
       {
       a;
       num x = (0);
       if (false) then {
           if ((3) xor (2))
           then
           {/=\\(\"kal\");};
       } else {num y = ((9));};
       };

       char ch = (\"p\");
       
       {a;b;c;};

       list_of list_of num func f (num a, string b, bool e)
       {
           num a = (list((3)(3)(4)));
           a[not(4)];
       };

       while ((string-append (\"a\") (\" bc\")) != (\"a bc\")){
          string-length (\"email: \");
       };
       
       }")

(define simple-program
      "
   { 
       num x = (0);
       num func f(num a, string b, bool e){
          if ((e)) then {
              (a);
          } else {
              x = (321);
              f(((a) - (10)) (b) (true));
          };
       };
       (42) - (542);
       f((4) (\"salamb\") (false));
       f((x) (\" \") (false));
       bool c = (true);
       while(c){
           num x = ((x) - (12));
           /=\\(x);
           c = (false);
       };
       /=\\(x);

     
   }       
")

(define sp
      "
   { 
   list_of num y = (list ((0) (3.54)));
   y = (cdr (y));
   num x = (car (y));
   y = (append (y) (list ((0) (3))));
   }       
")

(define input (open-input-string sp))
(define p (open-input-string program))
(define AST (full-parser (lex-this my-lexer input)))
(value-of-program AST)
|#