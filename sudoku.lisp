(defpackage :sudoku
    (:use :cl))

(in-package :sudoku)

(defun make-grid (contents)
  (make-array '(9 9) :initial-contents contents))

(defun clone-grid (old)
  (alexandria:copy-array old))

(defun at (grid row col)
  (aref grid row col))

(defun (setf at) (digit grid row col)
  (setf (aref grid row col) digit))

(defun row-digits (grid row)
  (let (digits)
    (dotimes (col 9 digits)
      (unless (zerop (at grid row col))
        (push (at grid row col) digits)))))

(defun col-digits (grid col)
  (let (digits)
    (dotimes (row 9 digits)
      (unless (zerop (at grid row col))
        (push (at grid row col) digits)))))

(defun box-digits (grid box-number)
  (multiple-value-bind (row-offset col-offset) (floor box-number 3)
    (loop with col-offset = (* 3 col-offset)
          with row-offset = (* 3 row-offset)
          for col from col-offset below (+ col-offset 3)
          nconc (loop for row from row-offset below (+ row-offset 3)
                      for digit = (at grid row col)
                      unless (zerop digit)
                        collect digit))))

(defun row-col-to-box (row col)
  (+ (* (floor row 3) 3)
     (floor col 3)))

(defun digits-possible-at (grid row col)
  (set-difference '(1 2 3 4 5 6 7 8 9)
                  (append (row-digits grid row)
                          (col-digits grid col)
                          (box-digits grid (row-col-to-box row col)))))

(defmacro do-unknowns ((row col box grid) &body body)
  `(dotimes (,row 9)
     (dotimes (,col 9)
       (when (zerop (at ,grid ,row ,col))
         (let ((,box (row-col-to-box ,row ,col)))
           (declare (ignorable ,box))
           ,@body)))))

(define-condition not-possible (error)
  ())

(defun scan (grid)
  (let (min
        row-min
        col-min
        possible-min
        (changed t))
    (loop
      (unless changed
        (return-from scan (list row-min col-min possible-min)))
      (setf changed nil
            row-min nil
            col-min nil
            possible-min nil
            min 10)
      (do-unknowns (row col box grid)
        (let ((possible (digits-possible-at grid row col)))
          (cond
            ((null possible)
             (error 'not-possible))
            ((null (cdr possible))
             (setf (at grid row col) (first possible)
                   changed t))
            (t
             (when (and (null changed)
                        (< (length possible) min))
               (setf min (length possible)
                     row-min row
                     col-min col
                     possible-min possible)))))))))

(defun solve (grid)
  (let ((grid (clone-grid grid)))
    (destructuring-bind (row col possible) (scan grid)
      (unless row
        (return-from solve grid))
      (dolist (digit possible)
        (setf (at grid row col) digit)
        (handler-case
            (return-from solve (solve grid))
          (not-possible ()
            ; catch and try next
            )))
      (error 'not-possible))))

(defun print-grid (grid)
  (dotimes (row 9)
    (format t "~{~A ~A ~A | ~A ~A ~A | ~A ~A ~A~}~%"
            (loop for col below 9 collect (at grid row col)))
    (when (member row '(2 5))
      (format t "------+-------+-------~%"))))