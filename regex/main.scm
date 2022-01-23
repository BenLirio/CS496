(define λ-2arg
  (lambda (x y)
    (if (symbol? x)
      (λ-abs (λ-var x) (λ y))
      (λ-exp (λ x) (λ y)))))

(define λ-1arg
  (lambda (x)
    (if (symbol? x)
      (λ-var x)
      (raise "Must be symbol"))))

(define λ
  (lambda args
    (if (null? args)
      (raise "At lease 1 arg")
      (if (null? (cdr args))
        (λ-1arg (car args))
        (if (null? (cdr (cdr args)))
          (λ-2arg (car args) (car (cdr args)))
          (raise "At most 2 args"))))))

(define λ?
  (lambda (x)
    (cond
      ((λ-var? x) #t)
      ((λ-abs? x) #t)
      ((λ-app? x) #t)
      (else #f))))

(define λ-var
  (lambda (x)
    (cons 'λ-var x)))
(define λ-var?
  (lambda (x)
    (cond
      ((pair? x) (if (eq? (car x) 'λ-var) #t #f))
      (else #f))))

(define λ-abs
  (lambda (x e)
    (if (λ-var? x)
      (if (λ? e)
        (cons 'λ-abs (cons x e))
        (raise "e must be lambda-exp"))
      (raise "x must be lambda-var"))))
(define λ-abs?
  (lambda (x)
    (cond
      ((pair? x) (if (eq? (car x) 'λ-abs) #t #f))
      (else #f))))

(define λ-app
  (lambda (e1 e2)
    (cons 'λ-app (cons e1 e2))))
(define λ-app?
  (lambda (x)
    (cond
      ((pair? x) (if (eq? (car x) 'λ-app) #t #f))
      (else #f))))


(define f
  (lambda args
    args))

(display (λ 'x 'y))
