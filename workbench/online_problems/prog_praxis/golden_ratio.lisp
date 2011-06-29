(defun golden-iter (n)
  (loop for i from 0 below n with g = 1
        do (setf g (1+ (/ g)))
        finally (return g)))

(format t "~S~%" (golden-iter 200))

(defun golden-rec (n)
  (if (zerop n)
      1
      (1+ (/ (golden-rec (1- n))))))

(format t "~S~%" (golden-rec 200))
