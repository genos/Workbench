; from
; http://blog.thezerobit.com/2012/09/01/beautiful-quicksort-in-common-lisp.html

; list comprehension
(defmacro @ (value bind list test)
  (let ((newlist (gensym)))
    `(let ((,newlist nil))
       (dolist (,bind ,list)
         (when ,test
           (push ,value ,newlist)))
       (nreverse ,newlist))))

; or
; (loop for bind in list when test collecting value)

(defgeneric lt (some other))

(defmethod lt ((some number) (other number)) (< some other))

(defmethod lt ((some string) (other string)) (string< some other))

(defun qsort (l)
  (when l (destructuring-bind (p . xs) l
            (append (qsort (@ x x xs (lt x p))) (list p)
                    (qsort (@ x x xs (not (lt x p))))))))

(qsort (list "this" "is" "only" "a" "test"))

(qsort (list 12/11 -1.09 3000))
