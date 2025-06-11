#lang racket

(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define (report-invalid-reference!) (eopl:error 'invalid-reference "\n\tillegal reference to memory!"))

(define the-store '())

(define free-pool '()) ; stores ref-val s not numbers. each member is of type of ref-val

(define get-free (lambda () (if (empty? free-pool) (ref-val (length the-store)) 
                                                    (let ((ref (car free-pool))) 
                                                        (begin (set! free-pool (cdr free-pool))
                                                                (set! the-store (append the-store (list 0)))
                                                                ref)))))
(define free-ref (lambda (ref) (cases expval ref (ref-val (num) (set! free-pool (append free-pool (list (ref-val num))))) 
    (else (report-invalid-reference!))))) ; ref should be of type of ref-val
; (define get-store (lambda () the-store))

(define initialize-store! (lambda () (begin
    (set! free-pool '())
    (set! the-store '())
 )))

(define newref (lambda (val) (let ((ref (get-free)))
    (begin
        (setref! ref val)
        (ref-val ref)))))

(define deref (lambda (ref) (let ((size (length the-store))) (cases expval ref (ref-val (num) (if (< num size) (list-ref the-store num) (report-invalid-reference!)))
                                                               (else (report-invalid-reference!))))))

(define setref! (lambda (ref val) (let ((size (length the-store))) (cases expval ref (ref-val (num) (if (< num size)
                                                                      (set! the-store (update num val the-store '())) (report-invalid-reference!)))
                                                                     (else (report-invalid-reference!))))))

(define update (lambda (num val store current) (cond [(= 0 num) (append current (list val) (cdr store))]
                                             [else (update (- num 1) val (cdr store) (append current (list (car store))))])))

(provide (all-defined-out))