(ql:quickload :cl-ppcre)

(defvar *test-string*
"
name:           Graham
quest:          life
number:         42
other:          17
one more:       1729
")

*test-string*

(cl-ppcre:split "\\n" *test-string*)

(rest (cl-ppcre:split "\\n" *test-string*))

(cl-ppcre:split "\\s+" (first *))

(mapcar #'(lambda (x) (cl-ppcre:split ":\\s+" x))
        (rest (cl-ppcre:split "\\n" *test-string*)))

(mapcar #'second *)

(defun test-func (name quest number other one-more)
  (cons name
        (cons quest
              (mapcar #'parse-integer (list number other one-more)))))

(defun pull-apart (s)
  (mapcar #'second
          (mapcar #'(lambda (x) (cl-ppcre:split ":\\s+" x))
                  (rest (cl-ppcre:split "\\n" s)))))

(ql:quickload :cl-interpol)

(in-package :cl-interpol)

(enable-interpol-syntax)

(in-package :cl-user)

(let ((a 42))
  #?"1234 != ${a}")

(defun output (s)
  (let ((x (pull-apart s)))
    (let ((name (first x))
          (quest (second x))
          (number (third x))
          (other (fourth x))
          (one-more (fifth x)))
      #?"
username:     ${name}
mission:      ${quest}
integer:      ${number}
next:         ${other}
finally:      ${one-more}
")))


(output *test-string*)

(ql:quickload 'cl-buchberger)

(in-package cl-buchberger)

(defparameter *ring*
  (make-instance 'polynomial-ring :variables (list 'x 'y 'z))
  "ℚ[X, Y Z]")

(describe '*ring*)

*ring*

(defparameter *l* (make-polynomial '((1 3 0) (-2 1 1))))

(defparameter *katsura-3*
             (make-ideal (list (make-polynomial '((1 1 0 0) (2 0 1 0) (2 0 0 1) (-1 0 0 0)))
                               (make-polynomial '((1 2 0 0) (-1 1 0 0) (2 0 2 0) (2 0 0 2)))
                               (make-polynomial '((2 1 1 0) (2 0 1 1) (-1 0 1 0)))))
             "Katsura-3 over ℚ[x, y, z] (default ring)")

*katsura-3*

(basis *katsura-3*)

(in-package :cl-user)
