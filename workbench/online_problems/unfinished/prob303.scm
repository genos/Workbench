(define (reduce f l id)
  (if (null? l)
      id
      (f (car l) (reduce f (cdr l) id)) ))

(define (range start finish)
  (if (>= start finish)
      '()
      (cons start (range (+ 1 start) finish)) ))

(define (member? x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
        (else (member? x (cdr l))) ))


(define (check n)
  (cond ((member? n '(0 1 2)) #t)
        ((not (member? (modulo n 10) '(0 1 2))) #f)
        (else (check (quotient n 10))) ))

(define (f n)
  (define (iter c)
    (if (check c)
        c
        (iter (+ c n)) ))
  (iter n) )

(define (f-div-n n)
  (quotient (f n) n) )

(display (reduce + (map f-div-n (range 1 10001)) 0))
(display "\n")
