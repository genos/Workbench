instance Num a =>
         Num [a] -- (1)
                        where
  (f:fs) + (g:gs) = f + g : fs + gs -- (2)
  fs + [] = fs -- (3a)
  [] + gs = gs -- (3b)
  (f:fs) * (g:gs) = f * g : [f] * gs + fs * (g : gs) -- (4)
  _ * _ = [] -- (5)
  abs = undefined -- I can't think of a sensible definition
  signum = map signum
  fromInteger n = [fromInteger n]
  negate = map (\x -> -x)
