;;; euler64.lisp
;;;
;;; hal22's lisp solution to Project Euler's problem #64

(defun get-cfrac-from-sqrt (n)
  (do* ((f (floor (sqrt n)) (floor (/ (+ (sqrt n) s) d)))
        (s f (abs (- s (* f d))))
        (d (- n (* s s)) (/ (- n (* s s)) d))
        (already (list (cons s d)) (cons (cons s d) already))
        (cf (list f) (cons f cf)))
       ((or (zerop d)
            (member (cons s d) (rest already) :test #'equal))
        (reverse cf))))