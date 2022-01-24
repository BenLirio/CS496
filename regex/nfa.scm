
(define A
  (lambda (x)
    (cond
      ((equal? x #\a) (cons A B))
      (else ()))))
(define B
  (lambda (x)
    (cond
      ((equal? x #\b) (cons B ()))
      (else ()))))

(define show
  (lambda (x)
    (if (null? x)
        ()
        ((display (car x)) (show (cdr x))))))

(display (show (cons A B)))
