; http://www.cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/

; to match words
(define true #t)
(define false #f)
(define empty? null?)
(define first car)
(define rest cdr)

; main macro
(define-syntax automaton
  (syntax-rules (:)
    ((_ init-state
        (state : response ...)
        ...)
     (let-syntax
       ((process-state
          (syntax-rules (accept ->)
            ((_ accept)
             (lambda (stream)
               (cond
                 ((empty? stream) true)
                 (else false))))
            ((_ (label -> target) (... ...))
             (lambda (stream)
               (cond
                 ((empty? stream) false)
                 (else
                   (case (first stream)
                     ((label) (target (rest stream)))
                     (... ...)
                     (else false)))))))))
       (letrec ((state
                  (process-state response ...))
                ...)
         init-state)))))

(define m
  (automaton init
             (init : (c -> more))
             (more : (a -> more)
                     (d -> more)
                     (r -> end))
             (end  : accept)))

(define run
  (lambda (xs)
    (display xs)
    (newline)
    (display (m xs))
    (newline)))

(for-each run
          '((c a d a)
            (c a d a r)
            (c a d a r r)))
