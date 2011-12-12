#!/usr/local/bin/sbcl --script
;;;; craps.lisp
;;;;
;;;; My submission to http://programmingpraxis.com/2011/11/04/craps/
;;;; Simulates games of Craps, outputs statistics; REQUIRES SBCL (*posix-argv*)
;;;; GRE, 11/4/11

(defun two-dice ()
  "The sum of rolling two dice"
  (+ (1+ (random 6)) (1+ (random 6))))

(defun crap (&optional (rounds 1) (rolls nil) (point 0))
  "A single round of craps"
  (let* ((roll (two-dice)) (rolls (cons roll rolls)))
    (if (zerop point)
        (cond ((member roll '(7 11)) (list 'win rounds rolls))
              ((member roll '(2 3 12)) (list 'lose rounds rolls))
              (t (crap (1+ rounds) rolls roll)))
        (cond ((= roll point) (list 'win rounds (reverse rolls)))
              ((= roll 7) (list 'lose rounds (reverse rolls)))
              (t (crap (1+ rounds) rolls point))))))
        
(defun max-index (seq)
  "Index with maximum in sequence"
  (let ((m (reduce #'max seq)))
    (position m seq)))

(defun craps (n)
  "Stats for n rounds of craps"
  (loop repeat n with rolls = (make-array 13 :initial-element 0)
     for c = (crap) then (crap)
     counting (eq 'win (first c)) into wins
     counting (eq 'lose (first c)) into losses
     summing (second c) into n-rounds
     maximizing (second c) into max-rounds
     do (loop for r in (third c) do (incf (elt rolls r)))
     finally (return (list wins losses n-rounds max-rounds (max-index rolls)))))

(defun craps-output (n)
  "Output statistics for n craps games"
  (let ((cs (coerce (craps n) 'vector)))
    (format t "~{~a: ~f~%~}" (list "Win percentage" (/ (elt cs 0) n)
                                   "Loss percentage" (/ (elt cs 1) n)
                                   "Average number of rounds" (/ (elt cs 2) n)))
    (format t "~{~a: ~d~%~}" (list "Maximum number of rounds" (elt cs 3)
                                   "Most common roll" (elt cs 4)))))

;;; Main event
(craps-output (read-from-string (second *posix-argv*)))