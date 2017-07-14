import Prelude hiding (gcd)

gcd :: Integral n => n -> n -> n
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

xgcd :: (Integral n) => n -> n -> (n, n)
xgcd _ 0 = (1, 0)
xgcd a b = (t, s - q * t)
 where
  (q, r) = a `divMod` b
  (s, t) = xgcd b r

modInv :: (Integral n) => n -> n -> n
modInv a b | gcd a b /= 1 = error "Not coprime"
           | otherwise    = inv a b
 where
  inv a b | f > 0     = f
          | otherwise = b + f
  f = fst $ xgcd a b
