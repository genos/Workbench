module Main where

data Expr a
  = Lit Integer
  | Add (Expr a)
        (Expr a)
  | Let (Expr a)
        (a -> Expr a)
  | Var a

eval :: Expr Integer -> Integer
eval (Lit d) = d
eval (Add e0 e1) = eval e0 + eval e1
eval (Let e f) = eval (f (eval e))
eval (Var v) = v

instance Num (Expr a) where
  fromInteger = Lit . fromInteger
  (+) = Add
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

tree
  :: (Num a, Eq a)
  => a -> Expr b
tree 0 = 1
tree n = Let (tree (n - 1)) ((\shared -> shared + shared) . Var)

text :: Expr String -> String
text e = go e (0 :: Integer)
  where
    go (Lit j) _ = show j
    go (Add e0 e1) c = "(Add " ++ go e0 c ++ " " ++ go e1 c ++ ")"
    go (Let e0 f) c =
      "(Let " ++ v ++ " " ++ go e0 (c + 1) ++ " in " ++ go (f v) (c + 1) ++ ")"
      where
        v = "v" ++ show c
    go (Var x) _ = x

main :: IO ()
main = do
  let t1024 :: Expr Integer = tree (1024 :: Integer)
  print . eval $ t1024
  let t3 :: Expr String = tree (3 :: Integer)
  putStrLn . text $ t3
