\* Counts all triples with perimeter less than n, given a method of generating
   primitive Pythagorean triples
*\
(load "shen-libs/maths-lib.shen")

(define neg
  N -> (- 0 N))

(define hall
  N -> (hall-1 N [3 4 5]) where (and (natural? N) (positive? N))
  _ -> [[]])

(define hall-1
  N [A B C] -> (if  (< N (+ A B C))
                    []
                    (append
                     [(if (< A B) [A B C] [B A C])]
                     (hall-1 N [(+ A (neg B) (neg B) C C)
                                (+ A A (neg B) C C)
                                (+ A A (neg B) (neg B) C C C)])
                     (hall-1 N [(+ A B B C C)
                                (+ A A B C C)
                                (+ A A B B C C C)])
                     (hall-1 N [(+ (neg A) B B C C)
                                (+ (neg A) (neg A) B C C)
                                (+ (neg A) (neg A) B B C C C)]))))

(define f
  Pyth N -> (sum (map (/. P (div N P)) (map sum (Pyth N)))))
