#lang racket
(require (lib "eopl.ss" "eopl"))

; (define N 100)

(define (report-out-of-memory!) (eopl:error 'newref "\n\tmemory is full!"))
(define (report-unauthorized-access-to-memory! var) (eopl:error 'unauthorized-access-to-memory "\n\tlocation ~s hasn't been allocated!" var))
(define (report-memory-index-out-of-range! idx N) (eopl:error 'memory-index-out-of-range "\n\tindex ~s isn't in range 0 to ~s!" idx (- N 1)))

(struct memory (size store free-addr-pool) #:mutable)
; (define-datatype memory memory?
;   (a-memory (size number?) (store vector?) (free-addr-pool list?)))
  
; (define store (make-vector N))
; (define free-addr-pool (range 0 (- N 1)))

; returns a memory struct, hopefully mutable
(define create-memory
  (lambda (N)
    (let ([store (make-vector N)] [free-addr-pool (range 0 N)])
      (memory N store free-addr-pool))))

(define in-range?
  (lambda (mem index)
    (and (>= index 0) (< index (memory-size mem)))))

(define newref
  (lambda (mem)
    (let ([free-addr-pool (memory-free-addr-pool mem)])
      (if (null? free-addr-pool) 
          (report-out-of-memory!)
          (let ([alloc-addr (first free-addr-pool)])
            (set-memory-free-addr-pool! mem (rest free-addr-pool))
            alloc-addr)))))

(define newref-init
  (lambda (mem init-val)
    (let ([index (newref mem)])
      (vector-set! (memory-store mem) index init-val)
      index)))

(define assign
  (lambda (mem index val)
    (cond
      [(not (in-range? mem index)) (report-memory-index-out-of-range! index (memory-size mem))]
      [(member index (memory-free-addr-pool mem)) (report-unauthorized-access-to-memory! index)]
      [else (vector-set! (memory-store mem) index val)])))

(define get-val
  (lambda (mem index)
    (cond
      [(not (in-range? mem index)) (report-memory-index-out-of-range! index (memory-size mem))]
      [(member index (memory-free-addr-pool mem)) (report-unauthorized-access-to-memory! index)]
      [else (vector-ref (memory-store mem) index)])))

(define free
  (lambda (mem index)
    (cond
      [(not (in-range? mem index)) (report-memory-index-out-of-range! index (memory-size mem))]
      [(member index (memory-free-addr-pool mem)) (report-unauthorized-access-to-memory! index)]
      [else (set-memory-free-addr-pool! mem (cons index (memory-free-addr-pool mem)))])))

(provide (all-defined-out))