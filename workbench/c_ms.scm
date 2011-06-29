(require-extension srfi-1)
(require-extension numbers)

;;; from http://codeimmersion.i3ci.hampshire.edu/2009/10/09/memoization/
(define memoize
  (lambda (f) ;; returns a memoized version of function f
    (let ((memo '()))
      (lambda args
        (let ((match (assoc args memo)))    ;; look up args
          (if match
              (cadr match)                  ;; return stored value
              (let ((value (apply f args))) ;; or calculate if necessary
                (set! memo                  ;; and store new value
                      (cons (list args value) memo))
                value)))))))

(define (range a b)
  (let loop ((x a) (acc '()))
    (if (>= x b)
        (reverse acc)
        (loop (+ 1 x) (cons x acc)))))

(define (prod lst) (fold-right * 1 lst))

(define (sum lst) (fold-right + 0 lst))

(define (b n k)
  (cond ((or (< k 0) (< n k)) 0)
        ((or (= k 0) (= k n)) 1)
        (else (/ (prod (range (+ (- n k) 1) n)) (prod (range 1 (+ k 1)))))))

(define (m n)
  (if (<= n 0)
      1
      (let ((c (/ 1 (* 2 (- (expt 3 n) 1))))
            (s (sum (map
                      (lambda (k) (* (b n k) (m (- n k)) (expt 2 k)))
                      (range 1 (+ 1 n))))))
        (* c s))))

(set! m (memoize m))

(define (main x)
  (begin
    (for-each (lambda (n) (display (m n)) (newline))
              (range 0 x))
    (exit)))

(main (string->number (car (command-line-arguments))))
