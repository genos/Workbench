{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, ScopedTypeVariables, EmptyDataDecls, FlexibleInstances #-}

module TentativeEdwards where

import Prelude
import Math.Core.Utils
import Math.Common.IntegerAsType
import Math.Algebra.Field.Base
import Math.Algebra.Field.Extension

data T25519
instance IntegerAsType T25519
    where
        value _ = 2^255 - 19

type F25519 = Fp T25519

f25519 :: [F25519]
f25519 = map fromInteger [0..(2^155 - 19)]

data Aff = Aff {xx :: F25519, yy :: F25519}
              deriving (Eq, Ord, Show)

instance Num Aff where
    (Aff x1 y1) + (Aff x2 y2) = Aff x3 y3 where
                 c = 1 :: F25519
                 d = (121665 :: F25519) / (121666 :: F25519)
                 w = d * x1 * y1 * x2 * y2
                 x3 = (x1 * y2 + y1 * x2) / (c * (1 + w))
                 y3 = (y1 * y2 - x1 * x2) / (c * (1 - w))
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger n = Aff (fromInteger n) (fromInteger n)
    negate (Aff x y) = Aff (negate x) y


infix 8 .*.
(.*.) :: F25519 -> Aff -> Aff
0 .*. _ = Aff 0 1
1 .*. a = a
n .*. a = a + (n - 1) .*. a

data P163
instance PolynomialAsType F2 P163
    where
        pvalue _ = convert $ x^163 + x^7 + x^6 + x^3 + 1
type F163 = ExtensionField F2 P163
f163 = map Ext (polys 163 f2) :: [F163]
a163 = embed x :: F163

data BAff = BAff {bx :: F163, by :: F163} deriving (Eq, Ord, Show)

instance Num BAff where
    (BAff x1 y1) + (BAff x2 y2) = BAff x3 y3 where
                  as = iterate (a163*) a163
                  cc = sum $ zipWith (*) as [1,0,0,0,0,1,0,1,1,0,0,1,1,1,1,1,0,
                                            0,0,1,1,1,1,0,0,1,0,1,1,1,1,0,0,0,
                                            0,0,1,1,0,0,0,1,1,0,1,0,1,0,0,0,1,
                                            1,1,0,0,1,0,1,1,1,0,0,1,0,0,0,1,0,
                                            0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,
                                            0,0,0,0,0,0,0,0,1,1,0,0,1,0,1,1,0,
                                            1,0,1,0,1,1,1,0,0,0,0,1,1,0,0,1,0,
                                            1,0,0,1,0,1,0,0,0,1,0,1,1,0,0,1,0,
                                            1,1,0,1,1,0,0,1,1,0,1,0,1,0,0,0,0,
                                            1,1,1,0,1,1,1,1,0,1]
                  dd = sum $ zipWith (*) as [0,0,0,1,1,0,1,0,1,0,0,1,0,1,1,0,0,
                                            1,0,1,1,0,0,0,0,0,0,0,1,0,1,0,1,1,
                                            1,0,0,0,1,1,0,0,0,0,0,1,0,0,1,1,1,
                                            0,1,1,1,0,0,0,0,1,0,1,0,1,0,1,0,1,
                                            1,0,0,1,0,1,0,0,1,0,0,0,1,1,0,0,1,
                                            1,0,1,1,0,0,0,1,0,0,1,0,1,0,0,0,1,
                                            0,0,1,0,0,0,0,1,0,1,1,0,0,0,1,0,1,
                                            1,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,
                                            1,1,0,0,0,1,0,1,0,0,1,1,1,1,0,1,0,
                                            1,0,0,0,1,1,1,1,1]
                  w1 = x1 + y1
                  w2 = x2 + y2
                  a = (x1 ^ 2) + x1
                  b = (y1 ^ 2) + y1
                  c = dd * w1 + w2
                  d = x2 * y2
                  x3 = y1 + (c + cc * (w1 + y2) + a * (d + x2)) / (cc + a * w2)
                  y3 = x1 + (c + cc * (w1 + y2) + b * (d + y2)) / (cc + b * w2)
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger n = BAff (fromInteger n) (fromInteger n)
    negate (BAff x y) = BAff y x

infix 8 /*\
(/*\) :: F163 -> BAff -> BAff
0 /*\ _ = BAff 0 0
1 /*\ a = a
n /*\ a = a + (n - 1) /*\ a
