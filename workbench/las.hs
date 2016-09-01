import Data.Digits
import Data.List

lookAndSay
  :: (Integral a)
  => [a]
lookAndSay = iterate l 1
  where
    l x = unDigits 10 $ concatMap c (group $ digits 10 x)
    c xs@(x:_) = [fromIntegral $ length xs, x]
