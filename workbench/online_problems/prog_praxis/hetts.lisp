;;; hetts.lisp
;;;
;;; My submission to http://programmingpraxis.com/2011/08/09/hetts-problem-128
;;; GRE, 8/10/11

(defun lsort (xss)
  (stable-sort (copy-list xss) #'< :key #'list-length))

(defun lfsort (xss)
  (let ((freq (make-hash-table)))
    (progn
      (dolist (xs xss) (incf (gethash (list-length xs) freq 0)))
      (stable-sort (copy-list xss) #'<
            :key #'(lambda (xs) (gethash (list-length xs) freq))))))

(let ((xss '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o))))
  (mapcar #'(lambda (s) (progn (print (funcall s xss)) (terpri)))
          '(identity lsort lfsort)))