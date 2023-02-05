(define zero (lambda () '()))
(define is-zero? (lambda (x) (null? x)))
(define next (lambda (x) (cons #t x)))
(define prev (lambda (x) (cdr x)))
(define show-aux
  (lambda (x n)
    (if (is-zero? x)
      (display n)
      (show-aux (prev x) (+ n 1)))))
(define show
  (lambda (x)
    (show-aux x 0)))

(show (next(next(zero))))
