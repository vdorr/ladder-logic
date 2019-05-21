
module Tooling where

import Data.Semigroup
import Data.List
import Data.Function

import Ladder.Lexer

--------------------------------------------------------------------------------

data D
    = R Int
--     | DD -- dummy
    deriving (Show, Eq, Ord)

data E op = Op op [D] -- and, or, ld #on, ...
    deriving Show

data V
    = X Bool 
    | I Int
    deriving (Show, Read, Eq)

data Op n s
    = And n -- wire out <- wire in and memory cell
--     | AndN
    | Ld    -- ^ set wire state same as argument
    | On    -- ^ set wire state to #on
    | St n
--     | StN | StOn | StOff
    | Jmp s
    | Cmp CmpOp n n
    -- | FB String [(String, D)] [(String, D)]
    deriving Show

data CmpOp = Lt | Gt | Lte | Gte | Eq | NEq
    deriving Show

--------------------------------------------------------------------------------

data LadderTest = T01
    { testVect :: [(Int, [(String, V)])]
    , watch :: [String]
    , expected :: [[V]]
    } deriving (Show, Read)

getPragma :: [Tok a] -> Maybe a
getPragma (Pragma p : xs) = Just p
getPragma (_ : xs)        = getPragma xs
getPragma _               = Nothing

tokens
    :: [(p, [((p, p), Tok a)])]
    -> [Tok a]
tokens = foldMap (fmap snd . snd)

--------------------------------------------------------------------------------

--generates one screen, chopping might be done outside
prettyTrace :: [(String, [V])] -> [String]
prettyTrace trace = x ++ ticks
    where
    ticks = case trace of
        ((_, l):_) -> [ pad "" ++ " " ++ replicate (length l) '╵']
        _          -> []

--     (names, values) = bimap (fmap pad) (fmap sparkline) $ unzip trace
    x = fmap (\(n, l) -> pad n ++ "|" ++ sparkline l) trace
    Max w = foldMap (Max . length . fst) trace
    pad s = replicate (w - length s) ' ' ++ s

-- "_▅_▅▅▅_"
sparkline :: [V] -> String
sparkline trace = fmap (bar.asInt) trace
    where
--     trace' = fmap asInt trace
    asInt (X True)  = 5
    asInt (X False) = 0
--     asInt (I i)     = i
    bar = ("_▂▃▄▅▆▇█" !!)

{-

  |_▅_▅▅▅_
  |_▅▅_▅__
  ╵ 
 ▕10

│ │ 
┼───
│ │ 

   │
V1 │_▂▃▄▅▆▇█________
  0|   ╵   ╵   ╵   ╵4s
V0 │_▂▃▄▅▆▇█▇▆▅▄▃▂__

   │
V1_│_▂▃▄▅▆▇█________
V0_│_▂▃▄▅▆▇█▇▆▅▄▃▂__
   ╵   ╵   ╵   ╵   ╵
              1s/div
-}

flattenTestVect :: TestVect -> [[(VarName, V)]]
flattenTestVect [] = []
flattenTestVect ((d, v) : xs)
    | d >= 1    = [v] ++ replicate (d - 1) [] ++ flattenTestVect xs
    | otherwise = flattenTestVect xs

updateMemory :: [(VarName, V)] -> [(VarName, V)] -> [(VarName, V)]
updateMemory old new = nubBy (on (==) fst) $ new ++ old --yeah performace be damned

type TestVect = [(Int, [(VarName, V)])]
type VarName = String

--------------------------------------------------------------------------------
