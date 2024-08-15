{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (Lens', makeLenses, use, uses, (%=), (+=), (.=), (^?))
import Control.Monad.Trans.State.Strict (State, evalState)
import Data.Attoparsec.Text (choice, decimal, parseOnly, sepBy1', signed)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.IO qualified as T

data Value
    = Exact {_vid :: {-# UNPACK #-} !Int, _value :: {-# UNPACK #-} !Int}
    | Unknown {_vid :: {-# UNPACK #-} !Int}
    | Input {_vid :: {-# UNPACK #-} !Int, _index :: {-# UNPACK #-} !Int}
    deriving (Show)
makeLenses ''Value

data Registers = Registers {_w :: !Value, _x :: !Value, _y :: !Value, _z :: !Value}
    deriving (Show)
makeLenses ''Registers

data Register = W | X | Y | Z deriving (Show)

reg :: Register -> Lens' Registers Value
reg = \case W -> w; X -> x; Y -> y; Z -> z

data Operator = Add | Mul | Div | Mod | Eql deriving (Show)

data Instruction
    = Inp Register
    | Bin {_operator :: !Operator, _destination :: !Register, _operand :: Either Int Register}
    deriving (Show)

type Program = [Instruction]

readProgram :: Text -> Program
readProgram = fromRight (error "Bad parse") . parseOnly (line `sepBy1'` "\n")
  where
    line = choice [Inp <$> ("inp " *> r), Bin <$> o <*> (" " *> r) <*> (" " *> lr)]
    o = choice ["add" $> Add, "mul" $> Mul, "div" $> Div, "mod" $> Mod, "eql" $> Eql]
    r = choice ["w" $> W, "x" $> X, "y" $> Y, "z" $> Z]
    lr = choice [Left <$> signed decimal, Right <$> r]

data InclusiveRange = Range {_lo :: {-# UNPACK #-} !Int, _hi :: {-# UNPACK #-} !Int}
    deriving (Show)

data ALU = ALU
    { _registers :: !Registers
    , _nextVID :: {-# UNPACK #-} !Int
    , _nextInput :: {-# UNPACK #-} !Int
    , _valueRanges :: !(IntMap InclusiveRange)
    }
    deriving (Show)
makeLenses ''ALU

create :: ALU
create =
    let rs = Registers (Exact 0 0) (Exact 1 0) (Exact 2 0) (Exact 3 0)
        vs = M.fromList [(v, Range 0 0) | v <- [0 .. 3]]
     in ALU rs 4 0 vs

newExact :: Int -> State ALU Value
newExact n = do
    v <- use nextVID
    nextVID += 1
    valueRanges %= M.insert v (Range n n)
    pure $! Exact v n

newUnknown :: InclusiveRange -> State ALU Value
newUnknown ns = do
    v <- use nextVID
    nextVID += 1
    valueRanges %= M.insert v ns
    pure $! Unknown v

newInput :: State ALU Value
newInput = do
    v <- use nextVID
    nextVID += 1
    i <- use nextInput
    nextInput += 1
    valueRanges %= M.insert v (Range 0 9)
    pure $! Input v i

valueRange :: Int -> State ALU InclusiveRange
valueRange v = uses valueRanges (M.! v)

eval :: Operator -> Value -> Value -> State ALU Value
eval Add l (Exact _ 0) = pure l
eval Add (Exact _ 0) r = pure r
eval Add (Exact _ l) (Exact _ r) = newExact $! l + r
eval Mul (Exact _ 0) _ = newExact 0
eval Mul _ (Exact _ 0) = newExact 0
eval Mul (Exact _ l) (Exact _ r) = newExact $! l * r
eval Mul l (Exact _ 1) = pure l
eval Mul (Exact _ 1) r = pure r
eval Div l (Exact _ 1) = pure l
eval Div l@(Exact _ 0) _ = pure l
eval Div (Exact _ l) (Exact _ r) = newExact $! l `div` r
eval Mod l@(Exact _ 0) _ = pure l
eval Mod _ r@(Exact _ 1) = pure r
eval Mod (Exact _ l) (Exact _ r) = newExact $! l `mod` r
eval Mod _ (Exact _ r) = newUnknown (Range 0 (r - 1))
eval Eql (Exact _ l) (Exact _ r) = if l == r then newExact 1 else newExact 0
eval Eql l r = do
    lRange <- valueRange (_vid l)
    rRange <- valueRange (_vid r)
    if (_lo lRange > _hi rRange) || (_lo rRange > _hi lRange) then newExact 0 else newUnknown (Range 0 1)
eval _ _ _ = newUnknown $! Range minBound maxBound

traverseProgram ::
    (Instruction -> State ALU a) ->
    (Instruction -> Value -> Value -> State ALU a) ->
    Program ->
    [a]
traverseProgram f g p = evalState (traverse step p) create
  where
    step i@(Inp r) = do
        v <- newInput
        registers . reg r .= v
        f i
    step i@(Bin operator dest operand) = do
        left <- use (registers . reg dest)
        right <- case operand of
            Left n -> newExact n
            Right r -> use (registers . reg r)
        new <- eval operator left right
        g i new left

constantPropagation :: Program -> Program
constantPropagation =
    catMaybes
        . traverseProgram
            (pure . Just)
            (\i new left -> pure $! if (new ^? value) == (left ^? value) then Nothing else Just i)

inspectRegisters :: Program -> [Registers]
inspectRegisters = traverseProgram (const (use registers)) (const . const . const (use registers))

main :: IO ()
main = do
    program <- readProgram <$> T.readFile "input.txt"
    print $ length program
    let optimized = constantPropagation program
    print $ length optimized
    traverse_ print $! inspectRegisters optimized
