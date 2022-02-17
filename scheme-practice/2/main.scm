; (all? pred lst) returns true if all the elements satisfy pred

(define pred
  (lambda (x)
    (if (eq? x #t)
        #t
        #f)))
(define all?
  (lambda (pred lst)
    (fold (lambda (a b) (and (pred a) b)) #t lst)))

(display (all? pred '(#t #f #t)))
