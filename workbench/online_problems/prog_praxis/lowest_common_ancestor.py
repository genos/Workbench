class BST(object):
    def __init__(self, val=None, right=None, left=None):
        self.val = val
        self.right = right
        self.left = left
        return None

    def __nonzero__(self):
        return self.val is not None

    def insert(self, item):
        if not self:
            self.val = item
        elif item < self.val:
            if self.left:
                return self.left.insert(item)
            else:
                self.left = BST(val=item)
        elif item > self.val:
            if self.right:
                return self.right.insert(item)
            else:
                self.right = BST(val=item)
        return None

    def lca(self, m, n):
        if n < self.val:
            return self.left.lca(m, n)
        elif m > self.val:
            return self.right.lca(m, n)
        else:
            return self.val


if __name__ == "__main__":
    b = BST()
    for k in [8, 3, 10, 1, 6, 14, 4, 7, 13]:
        b.insert(k)
    for pair in [(4, 7), (4, 10), (1, 4), (1, 3), (3, 6)]:
        print b.lca(*pair)

"""
First, a Python version. My <code>insert()</code> method is a bit cumbersome,
since it checks whether the current node has a left/right subtree before
heading down it. I owe the <code>lca()</code> implementation to Remco's
succinct Haskell solution.
[sourcecode lang="python"]
class BST(object):
    def __init__(self, val=None, right=None, left=None):
        self.val = val
        self.right = right
        self.left = left
        return None

    def __nonzero__(self):
        return self.val is not None

    def insert(self, item):
        if not self:
            self.val = item
        elif item < self.val:
            if self.left:
                return self.left.insert(item)
            else:
                self.left = BST(val=item)
        elif item > self.val:
            if self.right:
                return self.right.insert(item)
            else:
                self.right = BST(val=item)
        return None

    def lca(self, m, n):
        if n < self.val:
            return self.left.lca(m, n)
        elif m > self.val:
            return self.right.lca(m, n)
        else:
            return self.val

if __name__ == "__main__":
    b = BST()
    for k in [8, 3, 10, 1, 6, 14, 4, 7, 13]:
        b.insert(k)
    for pair in [(4, 7), (4, 10), (1, 4), (1, 3), (3, 6)]:
        print b.lca(*pair)
[/sourcecode]
I'm also trying to learn Common Lisp with the help of Paul Graham's "Ansi
Common Lisp;" here's a Common Lisp attempt:
[sourcecode lang="css"]
(defstruct (node) val (left nil) (right nil))

(defun insert (bst obj)
  (cond ((null bst) (make-node :val obj))
        ((= (node-val bst) obj) bst)
        ((> (node-val bst) obj)
         (make-node
           :val     (node-val bst)
           :left    (insert (node-left bst) obj)
           :right   (node-right bst)))
        (t
          (make-node
            :val    (node-val bst)
            :left   (node-left bst)
            :right  (insert (node-right bst) obj)))))

(defun lca (bst m n)
  (let ((v (node-val bst)) (l (node-left bst)) (r (node-right bst)))
    (cond ((and (not (null l)) (< n v)) (lca l m n))
          ((and (not (null r)) (> m v)) (lca r m n))
          (t v))))

(setf b nil)

(dolist (n '(8 3 10 1 6 14 4 7 13))
  (setf b (insert b n)))

(dolist (x '((4 . 7) (4 . 10) (1 . 4) (1 . 3) (3 . 6)))
  (print (lca b (car x) (cdr x))))

(princ #\newline)
[/sourcecode]
Both versions assume we're working with a binary search tree with only numbers
in it (or at least objects that can be compared with ==/=).
"""
