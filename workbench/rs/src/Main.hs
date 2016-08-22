{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import           Prelude                       hiding ( foldr, length, lookup
                                                      , mapM, replicate
                                                      , sequence )
import           Control.Applicative           ( (*>), (<$), (<$>), (<*), (<*>)
                                               , (<|>), empty, many, pure )
import           Control.Arrow                 ( (&&&), (***), (|||), first
                                               , second )
import           Control.Monad                 hiding ( mapM, sequence )
import           Control.Monad.Reader          hiding ( mapM, sequence )
import           Control.Monad.ST
import           Data.Foldable                 ( Foldable )
import qualified Data.Foldable                 as F
import           Data.List                     ( break )
import           Data.Map                      ( Map )
import qualified Data.Map                      as M
import           Data.Set                      ( Set )
import qualified Data.Set                      as S
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Numeric
import           Data.Bool.Extras              ( bool )
import           Data.Hashable
import           Data.HashTable.Class          ( HashTable )
import qualified Data.HashTable.ST.Cuckoo      as C
import qualified Data.HashTable.Class          as H
import           Text.ParserCombinators.Parsec hiding ( (<|>), count, many
                                                      , space )
import           Text.PrettyPrint.Leijen       ( (<+>), Doc, Pretty, pretty
                                               , space, text )
import qualified Text.PrettyPrint.Leijen       as PP

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = fmap fst &&& fmap snd

data Tree a = Empty
            | Leaf a
            | Node (Tree a) (Tree a)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = foldMap f l <> foldMap f r

count :: Foldable t => t a -> Int
count = getSum . foldMap (const $ Sum 1)

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

-- | The least fixpoint of functor f
newtype Fix f = Fix { unFix :: f (Fix f) }

-- | Derived instances for fixed functors, although this requires the
-- controversial @UndecidableInstances@ extension, amongst others
deriving instance Show (f (Fix f)) => Show (Fix f)

deriving instance Eq (f (Fix f)) => Eq (Fix f)

deriving instance Ord (f (Fix f)) => Ord (Fix f)

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

data ListF a r = C a r
               | N
    deriving (Show, Eq, Functor, Foldable, Traversable)

data NatF r = Succ r
            | Zero
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | catamorphism
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = c
  where
    c = alg . fmap c . unFix

data ExprF r = Const Int
             | Var Id
             | Add r r
             | Mul r r
             | IfNeg r r r
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- smart constructors
int :: Int -> Expr
int = Fix . Const

var :: Id -> Expr
var = Fix . Var

add :: Expr -> Expr -> Expr
add x y = Fix $ Add x y

mul :: Expr -> Expr -> Expr
mul x y = Fix $ Mul x y

ifNeg :: Expr -> Expr -> Expr -> Expr
ifNeg t x y = Fix $ IfNeg t x y

type Id = String

type Expr = Fix ExprF

type Env = Map Id Int

eval :: Env -> Expr -> Maybe Int
eval env = cata $ evalAlg env

evalAlg :: Env -> Algebra ExprF (Maybe Int)
evalAlg env = alg
  where
    alg (Const c) = pure c
    alg (Var i) = M.lookup i env
    alg (Add x y) = (+) <$> x <*> y
    alg (Mul x y) = (*) <$> x <*> y
    alg (IfNeg t x y) = t >>= bool x y . (< 0)

e1 :: Expr
e1 = mul (ifNeg (mul (int 1) (var "a"))
                (add (var "b") (int 0))
                (add (var "b") (int 2)))
         (int 3)

testEnv :: Env
testEnv = M.fromList [ ("a", 1), ("b", 3) ]

ppr :: Expr -> Doc
ppr = cata pprAlg

pprAlg :: Algebra ExprF Doc
pprAlg (Const c) = text $ show c
pprAlg (Var i) = text i
pprAlg (Add x y) = PP.parens $ x <+> text "+" <+> y
pprAlg (Mul x y) = PP.parens $ x <+> text "*" <+> y
pprAlg (IfNeg t x y) = PP.parens $
    text "ifNeg" <+> t <+> text "then" <+> x <+> text "else" <+> y

freeVars :: Expr -> Set Id
freeVars = cata alg
  where
    alg :: Algebra ExprF (Set Id)
    alg (Var i) = S.singleton i
    alg e = F.fold e

substitute :: Map Id Expr -> Expr -> Expr
substitute env = cata alg
  where
    alg :: Algebra ExprF Expr
    alg e@(Var i) = fromMaybe (Fix e) $ M.lookup i env
    alg e = Fix e

optAdd :: Algebra ExprF Expr
optAdd (Add (Fix (Const 0)) e) =
    e
optAdd (Add e (Fix (Const 0))) =
    e
optAdd e = Fix e

optMul :: Algebra ExprF Expr
optMul (Mul (Fix (Const 1)) e) =
    e
optMul (Mul e (Fix (Const 1))) =
    e
optMul e = Fix e

optimiseSlow :: Expr -> Expr
optimiseSlow = cata optAdd . cata optMul

optimiseFast :: Expr -> Expr
optimiseFast = cata (optMul . unFix . optAdd)

algProd :: Functor f => Algebra f a -> Algebra f b -> Algebra f (a, b)
algProd f g = (f *** g) . funzip

algCoProd :: (Functor f, Functor g)
          => Algebra f a
          -> Algebra g a
          -> Either (f a) (g a)
          -> a
algCoProd = (|||)

class Functor f => FixPoint f t | t -> f where
    inF :: Algebra f t -- f t -> t
    outF :: Coalgebra f t -- t -> f t

cataFP :: FixPoint f t => Algebra f a -> t -> a
cataFP alg = c
  where
    c = alg . fmap c . outF

instance Functor f => FixPoint f (Fix f) where
    inF = Fix
    outF = unFix

instance FixPoint (ListF a) [a] where
    inF N = []
    inF (C x xs) = x : xs
    outF [] = N
    outF (x : xs) = C x xs

instance FixPoint NatF Integer where
    inF Zero = 0
    inF (Succ n) = n + 1
    outF n
        | n > 0 = Succ (n - 1)
        | otherwise = Zero

-- | anamorphism
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = a
  where
    a = Fix . fmap a . coalg

-- | The greatest fixpoint of functor f
newtype Cofix f = Cofix { unCofix :: f (Cofix f) }

-- | An alternative anamorphism type for codata; we'd need this in Agda, Coq
ana' :: Functor f => Coalgebra f a -> a -> Cofix f
ana' coalg = a
  where
    a = Cofix . fmap a . coalg

data StreamF a r = S a r
    deriving (Show)

type Stream a = Cofix (StreamF a)

instance Functor (StreamF a) where
    fmap f (S x xs) = S x (f xs)

consS :: a -> Stream a -> Stream a
consS x xs = Cofix (S x xs)

headS :: Stream a -> a
headS (unCofix -> (S x _)) =
    x

tailS :: Stream a -> Stream a
tailS (unCofix -> (S _ xs)) =
    xs

dropS :: Int -> Stream a -> Stream a
dropS n s
    | n <= 0 = s
    | otherwise = dropS (n - 1) (tailS s)

takeS :: Int -> Stream a -> [a]
takeS n s
    | n <= 0 = []
    | otherwise = headS s : takeS (n - 1) (tailS s)

iterateS :: (a -> a) -> a -> Stream a
iterateS f = ana' c
  where
    c x = S x (f x)

-- | hylomorphism
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
-- hylo f g = cata f . ana g
hylo f g = h
  where
    h = f . fmap h . g

data LTreeF a r = LeafF a
                | BinF r r
    deriving (Eq, Ord, Show, Functor)

merge :: Ord a => LTreeF a [a] -> [a]
merge (LeafF x) = [ x ]
merge (BinF xs ys) = mergeList xs ys
  where
    mergeList [] ys = ys
    mergeList xs [] = xs
    mergeList (x : xs) (y : ys)
        | x <= y = x : y : mergeList xs ys
        | otherwise = y : x : mergeList xs ys

unflatten :: [a] -> LTreeF a [a]
unflatten [ x ] = LeafF x
unflatten (half -> (xs, ys)) =
    BinF xs ys

half :: [a] -> ([a], [a])
half xs = splitAt (F.length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort = hylo merge unflatten

-- | paramorphism
para :: FixPoint f t => (f (a, t) -> a) -> t -> a
-- para alg = fst . cataFP (alg &&& inF . fmap snd)
para alg = alg . fmap (para alg &&& id) . outF

fact :: Integer -> Integer
fact = para alg
  where
    alg Zero = 1
    alg (Succ (f, n)) = f * (n + 1)

sliding :: Int -> [a] -> [[a]]
sliding n = para alg
  where
    alg N = []
    alg (C x (r, xs)) = take n (x : xs) : r

cataTrace :: forall f a.
          (Functor f, Ord (f (Fix f)), Foldable f)
          => Algebra f a
          -> Fix f
          -> Map (Fix f) a
cataTrace alg = para phi
  where
    phi :: f (Map (Fix f) a, Fix f) -> Map (Fix f) a
    phi (funzip -> (fm, ft)) =
        M.insert k v m'
      where
        k = Fix ft
        v = alg $ fmap (m' M.!) ft
        m' = F.fold fm

-- | The coproduct of pattern functors f and g
data (f :+: g) r = Inl (f r)
                 | Inr (g r)
    deriving (Eq, Show, Functor)

-- | The product of pattern functors f and g
data (f :*: g) r = (f r) :*: (g r) deriving (Eq, Show, Functor)

-- | The free monad pattern functor
data FreeF f a r = FreeF (f r)
                 | Pure a
    deriving (Eq, Show, Functor)

-- | The cofree comonad pattern functor
data CofreeF f a r = CofreeF (f r) a
    deriving (Eq, Show, Functor)

-- | A context is a term (f r) which can contain holes a
data CtxF f a r = Term (f r)
                | Hole a
    deriving (Show, Functor)

-- | Context fixed-point type. A free monad.
type Ctx f a = Fix (CtxF f a)

-- | Deconstruct values of type @Ctx f a@
unCtx :: Ctx f a -> Either a (f (Ctx f a))
unCtx c = case unFix c of
    Hole x -> Left x
    Term t -> Right t

term :: f (Ctx f a) -> Ctx f a
term = Fix . Term

hole :: a -> Ctx f a
hole = Fix . Hole

-- | Fill all the holes of type @a@ in the template @Ctx f a@ using supplied
-- | function of type @ a -> Fix f@
fillHoles :: forall f a. Functor f => (a -> Fix f) -> Ctx f a -> Fix f
fillHoles g = cata alg where
    alg :: CtxF f a (Fix f) -> Fix f
    alg (Term t) = Fix t
    alg (Hole a) = g a

data JSValueF r = JSNull
                | JSBool Bool
                | JSNumber Double
                | JSString String
                | JSArray [r]
                | JSObject [(String, r)]
    deriving (Show, Eq, Ord, Functor, Foldable)

type JSValue = Fix JSValueF

parse' :: CharParser () a -> String -> a
parse' p = either (error . show) id . parse p "(unknown)"

type Name = String

pVar :: CharParser () Name
pVar = char '$' *> between (char '{') (char '}') (many alphaNum)

pJSTemplate :: CharParser () (Ctx JSValueF Name)
pJSTemplate = fix $ \p -> Fix <$> (Term <$> pJSValueF p <|> Hole <$> pVar)

temp1 :: Ctx JSValueF Name
temp1 = parse' pJSTemplate "[{\"foo\": ${a}}]"

vLookup :: Ord a => Map a JSValue -> a -> JSValue
vLookup env = fromMaybe (Fix JSNull) . (`M.lookup` env)

j1 :: Map String JSValue
j1 = M.fromList [("a", Fix $ JSNumber 42)]

-- | Annotate @(f r)@ with attribute @a@
newtype AnnF f a r = AnnF (f r, a) deriving Functor

-- | Annotated fixed-point type. A cofree comonad
type Ann f a = Fix (AnnF f a)

-- | Attribute of the root node
attr :: Ann f a -> a
attr (unFix -> AnnF (_, a)) = a

-- | Strip attribute from root
strip :: Ann f a -> f (Ann f a)
strip (unFix -> AnnF (x, _)) = x

-- | Strip all attributes
stripAll :: Functor f => Ann f a -> Fix f
stripAll = cata alg
  where
    alg (AnnF (x, _)) = Fix x

-- | Annotation constructor
ann :: (f (Ann f a), a) -> Ann f a
ann = Fix . AnnF

-- | Annotation desconstructor
unAnn :: Ann f a -> (f (Ann f a), a)
unAnn (unFix -> AnnF a) = a

synthesize :: forall f a. Functor f => Algebra f a -> Fix f -> Ann f a
synthesize f = cata alg where
    alg :: f (Ann f a) -> Ann f a
    alg = ann . (id &&& f . fmap attr)

sizes :: (Functor f, Foldable f) => Fix f -> Ann f Int
sizes = synthesize $ (+1) . F.sum

pprAnn :: Pretty a => Ann ExprF a -> Doc
pprAnn = cata alg
  where
    alg (AnnF (d, a)) = pprAlg d <+> text "@" <+> pretty a

inherit :: forall f a. Functor f => (Fix f -> a -> a) -> a -> Fix f -> Ann f a
inherit f root n = para alg n root
  where
    alg :: f (a -> Ann f a, Fix f) -> (a -> Ann f a)
    alg (funzip -> (ff, n)) p =
        ann (n', a)
      where
        a = f (Fix n) p
        n' = fmap ($ a) ff

depths :: Functor f => Fix f -> Ann f Int
depths = inherit (const (+1)) 0

cataM :: (Monad m, Traversable f) => (f a -> m a) -> Fix f -> m a
cataM algM = c
  where
    c = algM <=< (mapM c . unFix)

eval' :: Env -> Expr -> Maybe Int
eval' env = (`runReaderT` env) . cataM algM
  where
    algM :: ExprF Int -> ReaderT Env Maybe Int
    algM (Const c) = return c
    algM (Var i) = ask >>= lift . M.lookup i
    algM (Add x y) = return $ x + y
    algM (Mul x y) = return $ x * y
    algM (IfNeg t x y) = return $ bool x y (t < 0)

memoize :: Memo k v m => (k -> m v) -> k -> m v
memoize f x = lookup x >>=
    (`maybe` return) (f x >>= \r -> insert x r >> return r)

memoFix :: Memo k v m => ((k -> m v) -> k -> m v) -> k -> m v
memoFix f = mf
  where
    mf = memoize (f mf)

runMemo :: (forall s. ReaderT (C.HashTable s k v) (ST s) a) -> a
runMemo m = runST $ H.new >>= runReaderT m

-- could result in a slowdown unless your algebra is significantly more
-- expensive than a hash computation
memoCata :: (Eq (f (Fix f)), Traversable f, Hashable (Fix f))
         => Algebra f a
         -> Fix f
         -> a
memoCata f x = runMemo $ memoFix (\r -> fmap f . mapM r . unFix) x

-- | apomorphism
apo :: FixPoint f t => (a -> f (Either a t)) -> a -> t
apo coa = inF . fmap (apo coa ||| id) . coa

insertElem :: forall a. Ord a => ListF a [a] -> [a]
insertElem = apo c
  where
    c :: ListF a [a] -> ListF a (Either (ListF a [a]) [a])
    c N = N
    c (C x []) = C x (Left N)
    c (C x (y : xs))
        | x <= y = C x (Right (y : xs))
        | x > y = C y (Left (C x xs))

insertionSort :: Ord a => [a] -> [a]
insertionSort = cataFP insertElem

algZygo :: Functor f => Algebra f b -> (f (a, b) -> a) -> f (a, b) -> (a, b)
algZygo f g = g &&& f . fmap snd

zygo :: Functor f => Algebra f b -> (f (a, b) -> a) -> Fix f -> a
zygo f g = fst . cata (algZygo f g)

discontAlg :: ExprF (Sum Int, Maybe Int) -> Sum Int
discontAlg (IfNeg (t, tv) (x, xv) (y, yv))
    | isJust xv, isJust yv, xv == yv =
          t <> x <> y
    | otherwise = maybe (Sum 1 <> t <> x <> y)
                        (bool (t <> y) (t <> x) . (< 0))
                        tv
discontAlg e = F.fold . fmap fst $ e

-- | Number of live conditionals
disconts env = getSum . zygo (evalAlg env) discontAlg

e2 :: Expr
e2 = ifNeg (var "b") e1 (int 4)

-- | Histomorphism
histo :: FixPoint f t => (f (Ann f a) -> a) -> t -> a
histo alg = attr . cataFP (ann . (id &&& alg))

fib :: Integer -> Integer
fib = histo f
  where
    f :: NatF (Ann NatF Integer) -> Integer
    f Zero = 0
    f (Succ (unAnn -> (Zero, _))) =
        1
    f (Succ (unAnn -> (Succ (unAnn -> (_, n)), m))) =
        m + n

evens :: [a] -> [a]
evens = histo alg
  where
    alg N = []
    alg (C _ (strip -> N)) =
        []
    alg (C _ (strip -> C x y)) =
        x : attr y

futu :: Functor f => (a -> f (Ctx f a)) -> a -> Cofix f
futu coa = ana' ((coa ||| id) . unCtx) . hole

exch :: Stream a -> Stream a
exch = futu coa
  where
    coa xs = S (headS $ tailS xs)
               (term $ S (headS xs) (hole $ tailS $ tailS xs))
-- Runnable Main
main :: IO ()
main = do
    print $ eval testEnv e1
    print $ ppr e1
    print $ freeVars e1
    print $ freeVars . substitute (M.fromList [ ("b", var "a") ]) $ e1
    print $ ppr $ optimiseFast e1
    let s1 = iterateS (+ 1) 1
    print $ takeS 6 s1
    print $ msort [ 7, 6, 3, 1, 5, 4, 2 ]
    print $ fact 10
    print $ sliding 3 [1 .. 5]
    let m = cataTrace (evalAlg testEnv) $ optimiseFast e1
    print $ map (first ppr) $ M.toList m
    print $ fillHoles (vLookup j1) temp1
    print $ pprAnn $ sizes e1
    print $ pprAnn $ depths e1
    print $ freeVars e2
    print $ ppr . optimiseFast $ e2
    print $ disconts (M.fromList [ ("b", -1) ]) e2
    print $ fib 100
    print $ evens [1 .. 6]
    print $ takeS 10 $ exch s1
-- JSON Appendix
pJSValueF :: CharParser () r -> CharParser () (JSValueF r)
pJSValueF r = spaces *> pValue r

pSeries :: Char -> CharParser () r -> Char -> CharParser () [r]
pSeries left parser right =
    between (char left <* spaces) (char right) $
        (parser <* spaces) `sepBy` (char ',' <* spaces)

pArray :: CharParser () r -> CharParser () [r]
pArray r = pSeries '[' r ']'

pObject :: CharParser () r -> CharParser () [(String, r)]
pObject r = pSeries '{' pField '}'
  where
    pField = (,) <$> (pString <* char ':' <* spaces) <*> r

pBool :: CharParser () Bool
pBool = True <$ string "true" <|> False <$ string "false"

pValue :: CharParser () r -> CharParser () (JSValueF r)
pValue r = value <* spaces
  where
    value = choice [ JSString <$> pString
                   , JSNumber <$> pNumber
                   , JSObject <$> pObject r
                   , JSArray <$> pArray r
                   , JSBool <$> pBool
                   , JSNull <$ string "null"
                   ]

pNumber :: CharParser () Double
pNumber = getInput >>=
    (\s -> case readSigned readFloat s of
         [ (n, s') ] -> n <$ setInput s'
         _ -> empty)

pString :: CharParser () String
pString = between (char '\"') (char '\"') (many jchar)
  where
    jchar = char '\\' *> pEscape <|> satisfy (`notElem` "\"\\")

pEscape :: CharParser () Char
pEscape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
  where
    decode c r = r <$ char c

-- Memo Appendix
class Monad m => Memo k v m | m -> k, m -> v where
    lookup :: k -> m (Maybe v)
    insert :: k -> v -> m ()

-- | HashTable-based Memo monad
instance (Eq k, Hashable k, HashTable h) => Memo k v (ReaderT (h s k v) (ST s)) where
    lookup k = ask >>= \h -> lift $ H.lookup h k
    insert k v = ask >>= \h -> lift $ H.insert h k v

instance Hashable Expr where
    hashWithSalt s = F.foldl hashWithSalt s . unFix

instance Hashable r => Hashable (ExprF r) where
    hashWithSalt s (Const c) =
        1 `hashWithSalt` s `hashWithSalt` c
    hashWithSalt s (Var id) =
        2 `hashWithSalt` s `hashWithSalt` id
    hashWithSalt s (Add x y) =
        3 `hashWithSalt` s `hashWithSalt` (x, y)
    hashWithSalt s (Mul x y) =
        4 `hashWithSalt` s `hashWithSalt` (x, y)
    hashWithSalt s (IfNeg t x y) =
        5 `hashWithSalt` s `hashWithSalt` (t, x, y)
