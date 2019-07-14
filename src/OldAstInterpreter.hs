{-# LANGUAGE CPP, ViewPatterns, BangPatterns #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module OldAstInterpreter where

import Data.Foldable
import Data.List

import Language.Ladder.Zipper
-- import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Interpreter hiding (RW(..))
import Language.Ladder.Simple --FIXME should not be imported here

import Tooling

-- import TestUtils

--------------------------------------------------------------------------------

data D
    = R Int
--     | DD -- dummy
    deriving (Show, Eq, Ord)

data E op = Op op [D] -- and, or, ld #on, ...
    deriving Show

--------------------------------------------------------------------------------

fffff
    :: Eq p
    => Cofree (Diagram () (Op s (Operand String)) s) p
    -> ([(p, Int)], [(D, E (Op s (Operand String)))], Int)
fffff w@(p :< _a) =  ffff ([], [], 0) 0 p w

ffff
    :: Eq p
    => ([(p, Int)], [(D, E (Op s (Operand String)))], Int)
    -> Int
    -> p
    -> Cofree (Diagram () (Op s (Operand String)) s) p
    -> ([(p, Int)], [(D, E (Op s (Operand String)))], Int)
ffff (st, op, cnt) r src (p :< x) = f x
    where
    f (Source a) = 
        ffff
            (st
            , op <> [(R cnt, Op On [])] --getop r cnt s n
            , cnt + 1) cnt p a
    f  Sink = --end of hline, may lead to 'Node'
        ( st
        , op <> case lookup p st of
                     Nothing -> []
                     Just rr -> [ (R rr, Op Ld [R r]) ]
        , cnt
        )
    f  End = --end of vertical line
--should really appear only once at end of left power rail
--should test this (exactly one End in rung)
        (st, op, cnt)
    f (Device (dev) a) =
        ffff
            (st
            , op <> [(R cnt, Op dev [ R r ])] --getop r cnt s n
            , cnt + 1) cnt p a
--TODO at this point Jump should be handled separately, only basic blocks here
    f (Jump s) =
        (st, op <> [(R cnt, Op (Jmp s) [R r])], cnt + 1) --XXX XXX beware wires crossing jump point
    f (Node la) =
        foldl (\st' x' -> ffff st' r p x') (st <> [(p, r)], op, cnt) la
    f _ = error here

--------------------------------------------------------------------------------

evalTestVect
    :: [(D, [E (Op String (Operand String))])] -- ^network to evaluate
    -> [VarName]                      -- ^watched memory variables
    -> [(Int, [(VarName, V)])]        -- ^test vector (duration, stimuli)
    -> [[V]]       -- ^resulting trace, elems are same length as watch list
evalTestVect net = evalTestVect' (network' net)

--Foldable?
evalTestVect'
    :: ([(String, V)] -> [(VarName, V)]) -- ^network to evaluate
    -> [VarName]                      -- ^watched memory variables
    -> [(Int, [(VarName, V)])]        -- ^test vector (duration, stimuli)
    -> [[V]]       -- ^resulting trace, elems are same length as watch list
evalTestVect' prog watch vect = fst $ foldl step ([], []) vect'
    where

    vect' = flattenTestVect vect

    step (tr, mem) stim = (tr ++ [tr'], mem'')
        where
        mem' = updateMemory mem stim
        mem'' = prog mem'
        tr' = [ v | (flip lookup mem'' -> Just v) <- watch ]

--------------------------------------------------------------------------------

--TODO replace lookup by iorefs
network
    :: [(D, [E (Op String (Operand String))])]
    -> [(String, V)]
    -> ([(Int, Bool)], [(String, V)])
network net m0
    = foldl (\(m, r) -> f m r) ([], m0) net
    where
    f r m (dst, op) = g dst
        where
        g (R n) = (r ++ [(n, w')] , m1)
        (m1, w') = foldl h (m, False) op
        h (m', w) o = fmap (w ||) (rung m' r o)

network' net = snd . network net

-- rung :: Eq a0 => [(a0, Bool)] -> [(Int, Bool)] -> E (Op a0) -> ([(a0, Bool)], Bool)
rung m r (Op o a) = op o a
    where
    op (And (Var c)) [R n] = (m      , reg n && ldx c)
    op (St  (Var c)) [R n] = (st y c , y)
        where y = reg n
    op  Ld           [R n] = (m      , reg n)
    op  On           []    = (m      , True)
    op (Cmp Gt (Var a) (Var b)) [R n] = (m      , ldi a > ldi b)
    op (Cmp Gt (Var a) (Lit b)) [R n] = (m      , ldi a > b)
    op _       _     = error here

    reg n = case lookup n r of
                 Just v  -> v
                 Nothing -> error here

    mem f c
        = case zpLookup c (zpFromList m) of
            Zp l ((_c, v) : rs) -> (ret, zpToList (Zp l ((c, new) : rs)))
                where (ret, new) = f v
            _                   -> error $ show (here, c)

    ldx = fst . mem (\(X v) -> (v, X v)) --load bool
    ldi = fst . mem (\(I v) -> (v, I v)) --load bool
    st x = snd . mem (\v -> ((), X x))

--------------------------------------------------------------------------------

{-
--multiple assignments to same register are or-ed
--later --wires can cross JUMP, but power flow is strictly top->down
-}

or'd :: [(D, [a])] -> [(D, a)] -> [(D, [a])]
or'd out ((r@R{}, x) : xs)
    | Zp l ((d, a) : rs) <- zpLookup r (zpFromList out)
                       = or'd (zpToList (Zp l ((d, a ++ [x]) : rs))) xs
    | otherwise        = or'd ((r, [x]) : out) xs
-- or'd out ((d, x) : xs) = or'd ((d, [x]) : out) xs
or'd out []            = reverse out

--make tail recursive?
tsort :: [Int] -> [(D, [E op])] -> Maybe [(D, [E op])]
tsort _  [] = return []
tsort ks xs = do
    (yes@(_ : _), no) <- return $ partition (all test . snd) xs
    (yes ++) <$> tsort (ks ++ foldMap (getRegN . fst) yes) no
    where

    test (Op _ a) = all isIn a

    isIn (R n) = elem n ks
--     isIn _     = True

    getRegN (R n) = [n]
--     getRegN _     = []

--------------------------------------------------------------------------------

evalBlock :: [Instruction Int String]
          -> ItpSt String
          -> Either (ItpSt String, String) (ItpSt String)
evalBlock p st = foldlM eval st p

evalTestVect''
    :: [Instruction Int String] -- ^program
    -> [VarName] -- ^watched memory variables
    -> [(Int, [(VarName, V)])] -- ^test vector
    -> Either (Memory String, String) [[V]]
evalTestVect'' prog watch vect
    = case foldlM step ([], ([],[],[])) vect' of
        Left _ -> undefined
        Right (y, _) -> return y
    where

    vect' = flattenTestVect vect

    step (tr, st@(w, o, mem)) stim = do
        st'@(_, _, mem'') <- evalBlock prog (w, o, mem')
        let tr' = [ v | (flip lookup mem'' -> Just v) <- watch ]
        return (tr ++ [tr'], st')
        where
        mem' = updateMemory mem stim

--------------------------------------------------------------------------------

testAstOld :: Cofree (Diagram () (Dev String) String) DgExt -> IO ()
testAstOld ast' = do

    let watch = ["b", "d"]
    let memory =
                [ ("a", X True),("b", X False),("c", X False),("d", X False)
--                 , ("%QX0", X True), ("%IX0", I 0)
                ]

    xxx <- generateStk ast'
    for_ xxx print
    let watch2 = ["a","b","c","d"]
    let xxy = evalTestVect'' xxx watch2 vect01
    print (here, xxy)
    let Right tr2 = xxy
    putStrLn $ unlines $ prettyTrace $ zip watch2 $ transpose tr2

    let Right ast = parseOpsM ast'
    let (_st, op, _cnt) = fffff ast
    print (here, "-----------------------")
    let Just p01 = tsort [] $ or'd [] op
    print (here, "memory after single eval:", network' p01 memory)

    print (here, "-----------------------")
    for_ p01 print

    print (here, "-----------------------")
    print (here, "test trace:")

    let !trace = evalTestVect p01 watch vect01
    print (here, trace)
    putStrLn $ unlines $ prettyTrace $ zip watch $ transpose trace
