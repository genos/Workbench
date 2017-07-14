module Main where

class ExpAlg t  where
  lit :: Int -> t
  add :: t -> t -> t

e1 :: ExpAlg t => t
e1 = add (lit 1) (add (lit 2) (lit 3))

newtype Eval = Eval
  { eval :: Int
  }

instance ExpAlg Eval where
  lit n = Eval n
  add x y = Eval $ eval x + eval y

v1 :: Int
v1 = eval e1

class ExpAlg t =>
      MulAlg t  where
  mul :: t -> t -> t

e2 :: MulAlg t => t
e2 = mul (lit 4) (add (lit 5) (lit 6))

instance MulAlg Eval where
  mul x y = Eval $ eval x * eval y

v2 :: Int
v2 = eval e2

newtype View = View
  { view :: String
  }

instance ExpAlg View where
  lit n = View $ show n
  add x y = View $ "(" ++ view x ++ " + " ++ view y ++ ")"

s1 :: String
s1 = view e1

instance MulAlg View where
  mul x y = View $ "(" ++ view x ++ " * " ++ view y ++ ")"

s2 :: String
s2 = view e2

main :: IO ()
main = do
  mapM_ print    [v1, v2]
  mapM_ putStrLn [s1, s2]
