(define source
  (open-input-file "source.txt"))


(define A
  (lambda (x)
    (cond
      ((equal? x #\a) B)
      ((equal? x #\b) A)
      ((eq? x 'show) "A")
      (else (raise "A no match")))))

(define B
  (lambda (x)
    (cond
      ((equal? x #\a) A)
      ((equal? x #\b) C)
      ((eq? x 'show) "B")
      (else (raise "B no match")))))

(define C
  (lambda (x)
    (cond
      ((equal? x #\a) D)
      ((equal? x #\b) A)
      ((eq? x 'show) "C")
      (else (raise "C no match")))))

(define D
  (lambda (x)
    (cond
      ((equal? x #\a) A)
      ((equal? x #\b) A)
      ((eq? x 'show) "D")
      (else (raise "D no match")))))

(define F
  (lambda (s)
    (cond
      ((equal? s D) #t)
      (else #f))))


(define loop
  (lambda (p s)
    (display (s 'show))
    (p p (s (read-char source)))))
(loop loop A)
