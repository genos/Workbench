(defun arccot (x unity)
  ;;; Fixed point arithmetic and double recursion for arccot(x)
  (labels 
      ((arccot-minus (xsq n xpower)
         (let ((term (floor xpower n)))
           (if (= term 0)
               0
               (- (arccot-plus xsq (+ n 2) (floor xpower xsq))
                  term))))       
       (arccot-plus (xsq n xpower)
         (let ((term (floor xpower n)))
           (if (= term 0)
               0
               (+ (arccot-minus xsq (+ n 2) (floor xpower xsq))
                  term)))))
      (let ((xpower (floor unity x)))
           (arccot-plus (* x x) 1 xpower))))
    

(defun π-digit (n)
  ;;; Uses a Machin-Like formula to give the nth digit of π
  (let* ((unity (expt 10 (+ 10 n)))
         (π-thunk (* 4 (+
                       (* 183 (arccot 239 unity))
                       (* 32 (arccot 1023 unity))
                       (* (- 68) (arccot 5832 unity))
                       (* 12 (arccot 113021 unity))
                       (* (- 100) (arccot 6826318 unity))
                       (* (- 12) (arccot 33366019650 unity))
                       (* 12 (arccot 43599522992503626068 unity))))))
    (mod (floor π-thunk (expt 10 10)) 10)))


(defun check ()
  ;;; quick check
  (print (loop for n from 0 to 10 collect (π-digit n)))
  (terpri))


(defun main ()
  ;;; 1000th digits of π
  (print (π-digit 1000))
  (terpri)
  (terpri))


(check)
(main)
