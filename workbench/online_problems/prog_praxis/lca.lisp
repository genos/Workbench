(defstruct (node) val (left nil) (right nil))

(defun insert (bst obj)
  (cond ((null bst) (make-node :val obj))
        ((= (node-val bst) obj) bst)
        ((> (node-val bst) obj)
         (make-node
           :val     (node-val bst)
           :left    (insert (node-left bst) obj)
           :right   (node-right bst)))
        (t
          (make-node
            :val    (node-val bst)
            :left   (node-left bst)
            :right  (insert (node-right bst) obj)))))

(defun lca (bst m n)
  (let ((v (node-val bst)) (l (node-left bst)) (r (node-right bst)))
    (cond ((and (not (null l)) (< n v)) (lca l m n))
          ((and (not (null r)) (> m v)) (lca r m n))
          (t v))))

(setf b nil)

(dolist (n '(8 3 10 1 6 14 4 7 13))
  (setf b (insert b n)))

(dolist (x '((4 . 7) (4 . 10) (1 . 4) (1 . 3) (3 . 6)))
  (print (lca b (car x) (cdr x))))

(princ #\newline)
