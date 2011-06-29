"""primality_checking.lisp
My solution to http://programmingpraxis.com/2009/05/01/primality-checking/

GRE, 5/16/11
"""

(defun expt-mod (n exponent modulus)
  ;;; Fast modular exponentiation, from http://cliki.net/EXPT-MOD
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
        (setf result (mod (* result sqr) modulus))
        finally (return result)))


(defun r-and-s (n)
  ;;; r and s such that n = 2^r * s + 1
  (labels ((aux (x y)
                (if (or (oddp y) (zerop y))
                  (cons x y)
                  (aux (1+ x) (/ y 2)))))
    (aux 0 (1- n))))


(defun checkp (n)
  ;;; Single run of Miller-Rabin primality check
  (let* ((rns (r-and-s n))
         (r (car rns))
         (s (cdr rns))
         (a (1+ (random (1- n)))))
    (or (= 1 (expt-mod a s n))    ;; First Condition
        (some #'(lambda (j) (= (1- n) (expt-mod a (* s (expt 2 j)) n)))
              (loop for j from 0 to (1- r) collect j))))) ;; Second Condition


(defun miller-rabinp (n runs)
  ;;; Multiple runs of Miller-Rabin primality check
  (cond ((< n 2) nil)
        ((= n 2) t)
        ((evenp n) nil)
        (t (every #'(lambda (x) x)
                  (loop for k from 1 to runs collect (checkp n))))))


;;; Main event
(progn
  (print (miller-rabinp (1- (expt 2 89)) 50))
  (terpri)
  (terpri))
