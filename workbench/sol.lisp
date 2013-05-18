;;; Self-organizing lists

(defun greater-cdr (x y)
  "Compare the second element of two pairs."
  (> (cdr x) (cdr y)))

(defun assoc-update (x ys)
  "If x is in ys, update its count and return it. Otherwise, return nil."
  (let ((z (assoc x ys)))
    (if z
      (progn
        (incf (cdr z))
        (car z))
      nil)))

(defun reorder (ys)
  "A sorted copy of ys in descending order of second element."
  (stable-sort (copy-seq ys) #'greater-cdr))

(ql:quickload 'alexandria)

(defparameter *ys* (mapcar #'(lambda (x) (cons x 0)) (alexandria:iota 10)))

*ys*

(loop repeat 100 do (let ((x (random 20)))
                         (format t "~A?~8T~A~%" x (if
                                                   (assoc-update x *ys*)
                                                   t
                                                   nil))))

(setf *ys* (reorder *ys*))

*ys*
