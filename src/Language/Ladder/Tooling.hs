
module Language.Ladder.Tooling where

import Data.Semigroup
import Data.List
import Data.Function
import Data.Foldable
import Data.String
import Data.Bifunctor

import Language.Ladder.Utils
import Language.Ladder.Eval
import Language.Ladder.Types

--------------------------------------------------------------------------------

--generates one screen, chopping might be done outside
prettyTrace :: [(String, [V addr])] -> [String]
prettyTrace trace = x ++ ticks
    where
    ticks = case trace of
        ((_, l):_) -> [ pad "" ++ " " ++ replicate (length l) '╵']
        _          -> []

    x = fmap (\(n, l) -> pad n ++ "|" ++ sparkline l) trace
    Max w = foldMap (Max . length . fst) trace
    pad s = replicate (w - length s) ' ' ++ s

sparkline :: [V addr] -> String
sparkline [] = []
sparkline trace@(I _ : _) = fmap (bar.asInt) values
    where
    values = (`fmap` trace) $ \case
                                     I x -> x
                                     _ -> 0
    l = minimum values
    m = maximum values
    k = abs $ (m - l) `div` 6
    asInt i = (i - l) `div` k
sparkline trace@(X _ : _) = fmap (bar.asInt) trace
    where
    asInt (X True)  = 5
    asInt _ = 0
sparkline _ = undefined

bar :: Int -> Char
bar = ("_▂▃▄▅▆▇█" !!)

flattenTestVect :: TestVect addr -> [[(addr, V addr)]]
flattenTestVect [] = []
flattenTestVect ((d, v) : xs)
    | d >= 1    = [v] ++ replicate (d - 1) [] ++ flattenTestVect xs
    | otherwise = flattenTestVect xs

updateMemory :: Eq addr => [(addr, V addr)] -> [(addr, V addr)] -> [(addr, V addr)]
updateMemory old new = nubBy (on (==) fst) $ new ++ old --yeah performace be damned

type TestVect addr = [(Int, [(addr, V addr)])]
-- type VarName = String

-- |Returns names of signals in test vector
testVectSignals :: Eq addr => TestVect addr -> [addr]
testVectSignals = nub . foldMap (fmap fst . snd)

--------------------------------------------------------------------------------

evalTestVect
    :: (Eq addr, Show addr)
    => (addr -> st -> V addr)
    -> (st -> [(addr, V addr)] -> st)
    -> (st -> Either (st, String) st)
    -> st
    -> [addr] -- ^watched memory variables
    -> TestVect addr -- ^test vector
    -> Either (Memory addr, String) [[V addr]]
evalTestVect getTag setTag step st0 watch vect
    = case foldlM go ([], st0) vect' of
        Left  (_st, _err) -> error "evalTestVect" -- show (here, err)
        Right (y, _)     -> return y
    where

    vect' = flattenTestVect vect

    go (tr, st) stim = do
        st'' <- step st'
        let tr' = [ v | (flip getTag st'' -> v) <- watch ]
        return (tr ++ [tr'], st'')
        where
        st' = setTag st stim

--------------------------------------------------------------------------------

evalTestVect1
    :: (Eq addr, Show addr, IsString t, Eq t, Show t)
    => [(Maybe (Either Int t)
       , Cofree (Diagram c (DevType t, [Operand addr]) (Either Int t)) DgExt)]
    -> [addr]
    -> TestVect addr
    -> Either (Memory addr, String) [[V addr]]
evalTestVect1 prog = evalTestVect getTag setTag step st0
    where
    getTag addr (EvMem m) = maybe undefined id $ lookup addr m -- addr -> EvMem addr -> V addr
    setTag (EvMem m) new = EvMem $ updateMemory m new -- EvMem addr -> [(addr, V addr)] -> EvMem addr
    step m = first undefined $ evalM prog m
    st0 = EvMem []
--     eval blocks st0 = either undefined id (evalM blocks st0)

