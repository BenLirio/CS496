(define pred
  (lambda (x)
    (if (eq? x 3)
        #t
        #f)))

(define some?
  (lambda (pred lst)
    (cond
      ((null? lst)
       #f)
      ((pred (car lst))
       #t)
      (else
        (some? pred (cdr lst))))))
(define some2?
  (lambda (pred lst)
    (fold (lambda (a b) (or (pred a) b)) #f lst)))

(display (some2? pred '(1 2 3 4)))
