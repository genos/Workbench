{-
 - prng.hs
 -
 - A port of the PRNG code originally written in Python for my final project
 - in Applied Cryptography.
 - Most of these are derived from pseudocode given in Stinson's "Cryptography:
 - Theory and Practice," 3rd edition.
 -
 - GRE, 7/28/10
 -}
-- Fibonacci (-like) Generator
fib :: Integer -> Integer -> Integer -> [Integer]
fib m x y = x' : fib m y x' where x' = (x + y) `mod` m

-- Linear Congruential Generator
lcg :: Integer -> Integer -> Integer -> Integer -> [Integer]
lcg m a b x = x' `mod` 2 : lcg m a b x' where x' = (a * x + b) `mod` m

-- RSA Generator
rsa :: Integer -> Integer -> Integer -> [Integer]
rsa n b x = x' `mod` 2 : rsa n b x' where x' = (x ^ b) `mod` n

-- Discrete Logarithm Generator
dlg :: Integer -> Integer -> Integer -> [Integer]
dlg p a x = (if x' > p `div` 2 then 1 else 0) : dlg p a x'
  where
    x' = (a ^ x) `mod` p

-- Blum-Blum-Shub Generator
bbs :: Integer -> Integer -> [Integer]
bbs n x = x' `mod` 2 : bbs n x' where x' = (x ^ 2) `mod` n

-- helper #1 for bgEnc and bgDec
xor :: (Integral a) => a -> a -> a
xor a b = if a == b then 0 else 1

-- helper #2; adapted from Wikipedia's recursive version
xgcd :: (Integral a) => a -> a -> [a]
xgcd a b
    | a `mod` b == 0 = [b, 0, 1]
    | otherwise = [g, y, x - y * q]
  where
    q = a `div` b
    r = a `mod` b
    [g, x, y] = xgcd b r

-- Encryption in the Blum-Goldwasser Probabilistic PKC
bgEnc :: Integer -> Integer -> [Integer] -> ([Integer], Integer)
bgEnc n r xs = (zipWith xor xs bbsnr, (r ^ (2 ^ (k + 1) `mod` n)) `mod` n)
  where
    bbsnr = bbs n r
    k = length xs

-- Decryption in the Blum-Goldwasser Probabilistic PKC
bgDec :: Integer -> Integer -> [Integer] -> Integer -> [Integer]
bgDec p q ys s = zipWith xor ys bbsnr
  where
    n = p * q
    k = length ys
    [a, b] = drop 1 (xgcd p q)
    d1 = (((p + 1) `div` 4) ^ (k + 1)) `mod` (p - 1)
    d2 = (((q + 1) `div` 4) ^ (k + 1)) `mod` (q - 1)
    u = (s ^ d1) `mod` p
    v = (s ^ d2) `mod` q
    r = (v * a * p + u * b * q) `mod` n
    bbsnr = bbs n r

{- A quick run through bgEnc and bgDec to see that it works; a better test
 - suite is probably desirable. -}
main :: IO ()
main = do
    putStrLn "Test message: [1,1,0,0,1,0,1]."
    putStrLn "I'll encrypt with n = 272953 = 499 * 547 and r = 159201."
    putStrLn "\nEncrypted message:"
    print bgEncMessage
    putStrLn "\nJust to check, your decrypted message:"
    print bgDecMessage
  where
    xs = [1, 1, 0, 0, 1, 0, 1]
    p = 499
    q = 547
    n = p * q
    r = 159201
    bgEncMessage = bgEnc n r xs
    (ys, s) = bgEncMessage
    bgDecMessage = bgDec p q ys s
