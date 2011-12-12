;; ORIGINAL QUESTION
; Intersection of two sorted lists
; Output is also a sorted list; takes O(min(m, n)) time, where m = length(list1)
; and n = length(list2).
(define (intersection list1 list2)
  (let loop ((l1 list1) (l2 list2) (out '()))
    (if (or (null? l1) (null? l2))
        (reverse out)
        (let ((a (car l1)) (b (car l2)))
          (cond ((< a b) (loop (cdr l1) l2 out))
                ((> a b) (loop l1 (cdr l2) out))
                (else (loop (cdr l1) (cdr l2) (cons a out))))))))

;; BONUS #1
; Union of two sorted lists
; Output is also a sorted list; takes O(max(m, n)) time
(define (union list1 list2)
  (let loop ((l1 list1) (l2 list2) (out '()))
    (cond ((null? l1) (reverse (append (reverse l2) out)))
          ((null? l2) (reverse (append (reverse l1) out)))
          (else (let ((a (car l1)) (b (car l2)))
                  (cond ((< a b) (loop (cdr l1) l2 (cons a out)))
                        ((> a b) (loop l1 (cdr l2) (cons b out)))
                        (else (loop (cdr l1) (cdr l2) (cons a out)))))))))

;; BONUS #2
; If the lists aren't sorted, we need to sort them first for this to work.
; Using some asymptotically fast sort (like quicksort), we could end up with a
; solution that takes O(max(m, n) log(max(m, n))) time and constant space, if we
; just sort both lists in place before beginning. We could improve this by
; terminating the sort early once the shorter of the lists is sorted, if we were
; VERY careful.