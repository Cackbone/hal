;; Factorial
(define (fact x)
  (cond ((= x 1) 1)
        (#t (* x (fact (- x 1))))))

;; Exponent
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;; Fibonacci sequence
(define (fib x)
  (cond ((= x 0) 0)
        ((= x 1) 1)
        (#t (+ (fib (- x 1)) (fib (- x 2))))))

;; Is number odd
(define (odd? x) (= (mod x 2) 1))

;; Is number even
(define (even? x) (= (mod x 2) 0))

;; Is number positive
(define (positive? x) (< 0 x))

;; Is number negative
(define (negative? x) (> 0 x))

;; Is number zero
(define (zero? x) (= 0 x))
