;;; shamir_threshold_scheme.lisp
;;;
;;; Implements Adi Shamir's (k, n) threshold secret sharing scheme.
;;; GRE, 6/17/11


(defun prod (nums)
  "Product of nums"
  (reduce #'* nums :initial-value 1))

(defun expt-mod (n exponent modulus)
  "Fast modular exponentiation, from http://cliki.net/EXPT-MOD"
  (loop :with result = 1
        :for i :of-type fixnum :from 0 below (integer-length exponent)
        :for sqr = n :then (mod (* sqr sqr) modulus)
        :when (logbitp i exponent) :do
        (setf result (mod (* result sqr) modulus))
        :finally (return result)))


;; For mod inv, x^(phi(p) - 1) = x^{-1} mod p. Since p is prime, phi(p) = p - 1.
(defun mod-inv (x p)
  "Multiplicative inverse of x mod p. NOTE: p __must__ be prime."
  (expt-mod x (- p 2) p))


(defun horner-mod (x coeffs p)
  "Horner's rule to evaluate poly given by coeffs at x (modulo p)"
  (mod
    (reduce #'(lambda (coef acc) (mod (+ (* acc x) coef) p))
            coeffs
            :from-end t)
    p))


(defun sample (limit num)
  "Uniform sample of num integers from [1, limit), no repeats"
  (loop :with samp = '()
        :for x = (1+ (random (1- limit))) :then (1+ (random (1- limit)))
        :if (not (member x samp))
        :do (setf samp (cons x samp))
        :when (= num (length samp))
        :do (return samp)))


(defun shamir-threshold (s k n p)
  "Shamir's (k, n) threshold scheme"
  (let ((coeffs (cons s (loop :for i :from 0 :below (1- k)
                              :collect (1+ (random (1- p))))))
        (xs (sample p n)))
    (mapcar #'(lambda (x) (cons x (horner-mod x coeffs p))) xs)))


(defun interp-const (xy-pairs k p)
  "(shortcut) lagrangian interpolation to find constant term of poly given by
  points in xy-pairs"
  (if (< (length xy-pairs) k)
      (error "Not enough points for interpolation")
      (labels ((x (i) (car (nth i xy-pairs)))
               (y (i) (cdr (nth i xy-pairs)))
               (c (i) (mod (prod (loop :for j :from 0 :below k
                                       :if (not (= i j))
                                       :collect (mod (* (x j)
                                                        (mod-inv (- (x j)
                                                                    (x i))
                                                                 p))
                                                     p))) 
                           p)))
        (mod (loop :for i :from 0 :below k
                   :sum (mod (* (y i) (c i)) p))
             p))))


;;; Driver code
(let ((s #36rPRAXIS) (k 5) (n 20) (p 1557514061))
  (let ((xy-pairs (shamir-threshold s k n p)))
    (progn
      (print s)
      (dolist (pair xy-pairs) (print pair))
      (print (interp-const xy-pairs k p))
      (terpri))))