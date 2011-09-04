;;; euler57.lisp
;;;
;;; My solution (after cleaning up my original, slower one with help from the
;;; forum solutions) to Project Euler's problem 57.
;;; GRE, 8/5/11

(defun len-digits (n)
  "Number of digits in positive integer n"
  (loop for k = n then (floor k 10) for ds = 0 then (1+ ds) until (= k 0)
     finally (return ds)))

; The main event
(format t "~A~%" (loop for i from 0 below 1e3
                    for f = '(3 . 2) then (cons (+ (car f) (* 2 (cdr f)))
                                                (+ (car f) (cdr f)))
                    counting (> (len-digits (car f)) (len-digits (cdr f)))))

;; Norvig's solution:
;; (loop repeat 1000
;;     for f = 1 then (+ 1 (/ 1 (+ 1 f)))
;;     counting (> (length (princ-to-string (numerator f)))
;;                 (length (princ-to-string (denominator f)))))