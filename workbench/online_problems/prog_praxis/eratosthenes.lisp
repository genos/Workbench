"""eratosthenes.lisp
A Common Lisp version of the Sieve of Eratosthenes for 
http://programmingpraxis.com/2009/02/19/sieve-of-eratosthenes/2/
Really, I just expanded upon the version found here:
http://rosettacode.org/wiki/Sieve_of_Eratosthenes

GRE, 5/16/11
"""

(defun sieve (n)
  ;;; Sieve of Eratosthenes
  (let ((nums (make-array (1+ n) :element-type 'bit :initial-element 0)))
    (loop for i from 3 to n by 2
          when (zerop (bit nums i))
            collect i
            ;; Optimization 2: start sieving at i * i
            and do (loop for j from (expt i 2) to n by i
                         do (setf (bit nums j) 1)))))


(defun primes (n)
  ;;; Primes <= n
  (if (< n 2)
      '()
      ;; Optimization 1: only consider odds
      (cons 2 (sieve n))))


(defun main ()
  ;;; The main event
  (progn
    (print (primes 30))
    (print (length (primes 15485863)))
    (terpri)
    (terpri)))


(main)
