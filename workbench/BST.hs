import Data.List

data BST a = Empty | Node a (BST a) (BST a)
  deriving (Show, Read, Eq)

bstInsert :: (Ord a) => a -> BST a -> BST a
bstInsert x Empty = Node x Empty Empty
bstInsert x (Node y l r) | x == y = Node x l r
                         | x < y  = Node y (bstInsert x l) r
                         | x > y  = Node y l (bstInsert x r)

bstFromList :: (Ord a) => [a] -> BST a
bstFromList = foldr bstInsert Empty

bstFromList2 :: (Ord a) => [a] -> BST a
bstFromList2 = foldl' (flip bstInsert) Empty

bstFind :: (Ord a) => a -> BST a -> Maybe (BST a)
bstFind x Empty = Nothing
bstFind x (Node y l r) | x == y = Just (Node y l r)
                       | x < y  = bstFind x l
                       | x > y  = bstFind x r

bstMin :: (Ord a) => BST a -> a
bstMin (Node x Empty r) = x
bstMin (Node x l     r) = bstMin l

bstMax :: (Ord a) => BST a -> a
bstMax (Node x l Empty) = x
bstMax (Node x l r    ) = bstMax r

depth :: (Ord a, Integral b) => BST a -> b
depth Empty        = 1
depth (Node _ l r) = 1 + max (depth l) (depth r)

countEmpty :: (Ord a, Integral b) => BST a -> b
countEmpty Empty        = 1
countEmpty (Node x l r) = countEmpty l + countEmpty r

bstSum :: (Num a, Ord a) => BST a -> a
bstSum Empty        = 0
bstSum (Node x l r) = x + bstSum l + bstSum r

preorder :: (Ord a) => BST a -> [a]
preorder Empty        = []
preorder (Node x l r) = [x] ++ preorder l ++ preorder r

inorder :: (Ord a) => BST a -> [a]
inorder Empty        = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

postorder :: (Ord a) => BST a -> [a]
postorder Empty        = []
postorder (Node x l r) = postorder l ++ postorder r ++ [x]
