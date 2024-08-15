module Main where

data Expr a = Lit Integer | Add (Expr a) (Expr a) | Let (Expr a) (a -> Expr a) | Var a

eval :: Expr Integer -> Integer
eval = \case
    (Lit d) -> d
    (Add e0 e1) -> eval e0 + eval e1
    (Let e f) -> eval (f (eval e))
    (Var v) -> v

tree :: (Num a, Eq a) => a -> Expr b
tree 0 = Lit 1
tree n = Let (tree (n - 1)) ((\shared -> Add shared shared) . Var)

text :: Expr String -> String
text e = go e (0 :: Integer)
  where
    go (Lit j) _ = show j
    go (Add e0 e1) c = "(Add " <> go e0 c <> " " <> go e1 c <> ")"
    go (Let e0 f) c =
        let v = "v" <> show c
            c' = succ c
         in "(Let " <> v <> " " <> go e0 c' <> " in " <> go (f v) c' <> ")"
    go (Var x) _ = x

main :: IO ()
main = do
    let t1024 :: Expr Integer = tree @Integer 1024
    print . eval $ t1024
    let t3 :: Expr String = tree @Integer 3
    putStrLn . text $ t3
