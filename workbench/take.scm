(define take
  (lambda (n xs)
    (if (or (eq? xs '()) (<= n 0))
        '()
        (cons (car xs) (take (cdr xs) (- n 1))))))
