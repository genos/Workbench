;; Question 1: reverse a string
; probably cheating, but I can't find a better way
(defun string-rev (str)
  (reverse str))


;; Question 2: calculate the nth Fibonacci number
(defun fib-loop (n)
  (loop :for i :from 0 :below n
     :with fibs = '(0 1)
     :do (setf fibs (list (second fibs) (+ (first fibs) (second fibs))))
     :finally (return (first fibs))))

(defun fib-do (n)
  (do ((i 0 (1+ i))
        (fibs '(0 1) (list (second fibs) (+ (first fibs) (second fibs)))))
       ((= i n) (first fibs))))


;; Question 3: print the multiplication table up to 12
(defun mult-table-12-loop ()
  (loop :for a :from 1 :to 12 :do
     (loop :for b :from 1 :to 12 :do
        (format t "~6d" (* a b)))
     (terpri)))

(defun mult-table-12-dotimes ()
  (dotimes (a 12)
    (dotimes (b 12)
      (format t "~6d" (* (1+ a) (1+ b))))
    (terpri)))


;; Question 4: write a function that sums up integers from a text file, one per
;; line
(defun file-sum (file-name)
  (with-open-file (stream file-name)
    (loop :for line = (read-line stream nil 'EOF)
       :until (eq line 'EOF)
       :sum (parse-integer line))))


;; Question 5: write a function that prints the odd numbers from 1 to 99
(defun odds-1-to-99-loop ()
  (loop :for i :from 1 :to 99 :by 2 :do (print i)))

(defun odds-1-to-99-dotimes ()
  (dotimes (i 50) (print (1+ (ash i 1)))))


;; Question 6: find the largest int value in an int array
; cheating: (defun max-int (arr) (apply #'max (coerce arr 'list)))
(defun max-int-loop (arr)
  (loop :for i :in (coerce arr 'list) :maximize i))

(defun max-int-reduce (arr)
  (reduce #'(lambda (a b) (if (> a b) a b)) (coerce arr 'list)))


;; Question 7: format an RGB value (three 1-byte numbers) as a 6-digit
;; hexadecimal string
(defun rgb-to-hex (r g b)
  (flet ((to-hex (n)
           (let ((h (write-to-string n :base 16)))
             (if (= 2 (length h)) h (concatenate 'string "0" h)))))
    (apply #'concatenate 'string (mapcar #'to-hex (list r g b)))))