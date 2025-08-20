#lang racket
(require (lib "eopl.ss" "eopl"))

(define N 100)

(define (report-out-of-memory!) (eopl:error 'newref "\n\tmemory is full!"))
(define (report-unauthorized-access-to-memory! var) (eopl:error 'unauthorized-access-to-memory "\n\tlocation ~s hasn't been allocated!" var))
(define (report-memory-index-out-of-range! var) (eopl:error 'memory-index-out-of-range "\n\tindex ~s isn't in range 0 to ~s!" var (- N 1)))


(define memory (make-vector N))
(define free-addr-pool (range 0 (- N 1)))


(define in-range?
  (lambda (index)
    (and (>= index 0) (< index N))))

(define newref
  (lambda ()
    (if (null? free-addr-pool) 
        (report-out-of-memory!)
        (let ([alloc-addr (first free-addr-pool)])
          (set! free-addr-pool (rest free-addr-pool))
          alloc-addr))))

(define newref-init
  (lambda (init-val)
    (let ([index (newref)])
      (vector-set! memory index init-val)
      index)))

(define assign
  (lambda (index val)
    (cond
      [(not (in-range? index)) (report-memory-index-out-of-range! index)]
      [(member index free-addr-pool) (report-unauthorized-access-to-memory! index)]
      [else (vector-set! memory index val)])))

(define get-val
  (lambda (index)
    (cond
      [(not (in-range? index)) (report-memory-index-out-of-range! index)]
      [(member index free-addr-pool) (report-unauthorized-access-to-memory! index)]
      [else (vector-ref memory index)])))

(define free
  (lambda (index)
    (cond
      [(not (in-range? index)) (report-memory-index-out-of-range! index)]
      [(member index free-addr-pool) (report-unauthorized-access-to-memory! index)]
      [else (set! free-addr-pool (cons index free-addr-pool))])))

(provide (all-defined-out))