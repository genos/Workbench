(defmacro lcomp
  (expression for var in list conditional conditional-test)
  (let ((result (gensym)))
    `(let ((,result nil))
       (loop for ,var in ,list
             ,conditional
             ,conditional-test
             do (setq ,result (append ,result (list ,expression))))
       ,result)))

(defun range (n)
  (loop for i to n collecting i))

(lcomp (* x x) for x in (range 10) if (= (mod x 2) 0))
