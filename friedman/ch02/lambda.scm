(define var-exp
  (lambda (var)
    (cons 'var-exp var)))
(define var-exp?
  (lambda (e)
    (if (eq? (car e) 'var-exp)
        #t
        #f)))
(define var-exp->var (lambda (x) (cdr x)))

(define lambda-exp
  (lambda (var e)
    (cons 'lambda-exp (cons var e))))

(define lambda-exp?
  (lambda (e)
    (if (eq? (car e) 'lambda-exp) #t #f)))

