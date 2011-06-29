;;; rws.scm
;;;
;;; What amount of red socks and white socks are needed to reach a certain
;;; probability of 2 red socks when drawing 2 without replacement?

;; Simulation, adjusting sock count accordingly as we go
(define (rws-sim prob iter)
  (define (single-iter reds whites count)
    (cond ((> count iter)
            (newline) (display "Finished iterations.") (newline) )
          ((= (actual-prob reds whites) prob)
            (display-prob reds whites)
            (single-iter (+ reds 1) whites (+ count 1)) )
          ((< (actual-prob reds whites) prob)
            (single-iter (+ reds 1) whites (+ count 1)) )
          (else
            (single-iter reds (+ whites 1) (+ count 1))) ))
  (single-iter 1 1 0) )

;; Compute actual probability given two quantities
(define (actual-prob x y) (/ (* x (- x 1)) (* (+ x y) (+ x  y -1))))

;; Printing a solution
(define (display-prob x y)
  (display "# Red socks:\t") (display x) (newline)
  (display "# White socks\t") (display y) (newline)
  (display "------------------------------------------")(newline) )


;; Run it, with audience participation
(display "What probability should we target?") (newline)
(define p (read))
(display "How many iterations should we run?") (newline)
(define i (read))
(display "Ok, running simulations") (newline)(newline)
(rws-sim p i)
