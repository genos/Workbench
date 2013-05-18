(defconstant +koch+ (list 'f '+ '+ 'f '+ '+ 'f))

(defun rewrite (k)
  (mapcan #'(lambda (m)
              (case m
                ((f) (list 'f '- 'f '+ '+ 'f '- 'f))
                (t (list m))))
          k))

(defun koch-iter (i)
  (loop repeat i for k = +koch+ then (rewrite k) finally (return k)))

(defun koch-rec (i &optional (k +koch+))
  (if (<= i 0)
      k
      (koch-rec (1- i) (rewrite k))))

(defun to-logo (k)
  (with-output-to-string (s)
    (mapcar #'(lambda (m)
                (case m
                  ((f) (format s "fd 5~%"))
                  ((+) (format s "rt 60~%"))
                  ((-) (format s "lt 60~%"))))
            k)))

(print (to-logo (koch-rec 3)))