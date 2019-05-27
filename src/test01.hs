{-# OPTIONS_GHC -Wunused-imports #-}

{-# LANGUAGE CPP, TupleSections, TypeSynonymInstances, FlexibleInstances,
    QuasiQuotes, PatternSynonyms,TypeApplications,
    LambdaCase, ScopedTypeVariables, ViewPatterns, BangPatterns, FlexibleContexts #-}

-- OverloadedStrings, 

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
-- import qualified Data.Text as T
import Data.Foldable
-- import Data.Traversable
import Data.List
-- import Text.Read
-- import Data.Function
-- import Data.Bifunctor
import System.Environment (getArgs)
-- import Data.Tuple
-- import Control.Monad (replicateM_)
-- import Data.Semigroup

import Data.Maybe
import Control.Monad.Writer.Strict

-- import Debug.Trace

import Preprocess

import Ladder.Zipper
import Ladder.Lexer -- (preproc5', runLexer, dropWhitespace)
import Ladder.DiagramParser
import Ladder.LadderParser

-- import NeatInterpolation

import Tooling

--------------------------------------------------------------------------------

--TODO
--convert to some intermediate representation usable outside interpreter
--that is - extend with node numbering

fffff
    :: Eq p
    => Cofree (Diagram () (Op Operand s) s) p
    -> ([(p, Int)], [(D, E (Op Operand s))], Int)
fffff w@(p :< _a) =  ffff ([], [], 0) 0 p w

ffff
    :: Eq p
    => ([(p, Int)], [(D, E (Op Operand s))], Int)
    -> Int
    -> p
    -> Cofree (Diagram () (Op Operand s) s) p
    -> ([(p, Int)], [(D, E (Op Operand s))], Int)
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

--------------------------------------------------------------------------------

evalTestVect
    :: [(D, [E (Op Operand String)])] -- ^network to evaluate
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
    :: [(D, [E (Op Operand String)])]
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


testAst :: Cofree (Diagram () Dev String) DgExt -> IO ()
testAst ast' = do

    let watch = ["b", "d"]
    let memory =
                [ ("a", X True),("b", X False),("c", X False),("d", X False)
--                 , ("%QX0", X True), ("%IX0", I 0)
                ]
#if 0
    xxx <- generateStk ast'
    for_ xxx print
    let watch2 = ["a","b","c","d"]
    let xxy = evalTestVect'' xxx watch2 vect01
    print (here, xxy)
    let Right tr2 = xxy
    putStrLn $ unlines $ prettyTrace $ zip watch2 $ transpose tr2
#endif


    generateStk2 ast'
    return ()

#if 0

    let ast = parseOps ast'
    let (st, op, cnt) = fffff ast
--     print (here, "-----------------------")
--     for_ st print
--     print (here, "-----------------------")
--     for_ op print
    print (here, "-----------------------")
--     Just w <- return $ tsort [] $ or'd [] op
--     for_ (w) print
--     print (here, "-----------------------")
    let Just p01 = tsort [] $ or'd [] op
    print (here, "memory after single eval:", network' p01 memory)

    print (here, "-----------------------")
    for_ p01 print

    print (here, "-----------------------")
    print (here, "test trace:")

    let !trace = evalTestVect p01 watch vect01
    print (here, trace)
    putStrLn $ unlines $ prettyTrace $ zip watch $ transpose trace
#endif

--------------------------------------------------------------------------------

dropEnd
    :: Cofree (Diagram c d s) p
    -> Cofree (Diagram c d s) p
dropEnd (p :< a) = p :< f a
    where
    f (Node ns) = Node (fmap dropEnd (filter notEnd ns))
    f n         = fmap dropEnd n
    notEnd (_ :< End) = False
    notEnd _          = True

generateStk2 :: Cofree (Diagram () Dev String) DgExt -> IO [Instruction String Int]
generateStk2 ast' = do
    let ast = parseOps $ dropEnd ast'

    print (here, "-----------------------")
    print (here, "-----------------------")
    print (here, "-----------------------")

    --collapse nodes
    let (nodes, a0) = merge' ast

    --chop
    let Just (a1 ::[Cofree (Diagram () (Op Operand String) String) DgExt])
            = forest a0
--     let x1_x = ldlines'' [ast]
--     let x1_xxx = ldlines'' x1_0

    for_ a1 print
    print (here, "-----------------------")

    let a2 = ldlines'' a1
    let a3 = sortOn (\(p:<_) -> p) a2

    for_ a3 print
    print (here, "-----------------------")

--     let x1 = x1_x
#if 0
    let x1 = x1_0
--     for_ x1_0 print
--     print (here, "-----------------------")

    --collect stubs (per each forest tree)
    let x1' -- :: [([DgExt], Cofree (Diagram c (Op Operand String) String) DgExt)]
            = fmap (\x -> (stubs x, x)) x1
    --merge neighbouring nodes
    let q -- :: [(([DgExt], [(DgExt, [DgExt])]), Cofree (Diagram DgExt (Op Operand String) String) DgExt)]
            = fmap (\(stbs, tre) -> let (nds, tre') = merge' tre
                in ((stbs, nds), tre')
                    --this is result - stubs in subtree, merged nodes and new tree
                    ) x1'

    let allStubs = foldMap (fst . fst) q --aka sinks
    let nodesMerged :: [(DgExt, [DgExt])] = foldMap (snd . fst) q
    let allNodes = nub $ fmap fst nodesMerged ++ foldMap snd nodesMerged
    let sinksLeadingToNodes = filter (flip elem allNodes) allStubs --aka sinks
    print (here, "allNodes:", allNodes)
    print (here, "nodesMerged:", nodesMerged)
    print (here, "sinksLeadingToNodes:", sinksLeadingToNodes)
    let oldNodeToNewNode = nodeTable nodesMerged
    let nodeToSink
            = fmap (\p -> (fromJust $ lookup p oldNodeToNewNode, p)) sinksLeadingToNodes
    print (here, "nodeToSink:", nodeToSink)

    print (here, "-----------------------")
    for_ q $ \((stubs, _), tr) -> do
        print $ filter (flip elem allNodes) stubs
        print tr

    print (here, "-----------------------")
    let subTrees = fmap (\((stubs, _), tr) -> (stubs, tr)) q
--     generate (flip (for_ @[]) (print @Instruction)) nodeToSink
--         subTrees
    execWriterT $ generate tell nodeToSink subTrees
#else
    return []
#endif

--------------------------------------------------------------------------------

main :: IO ()
main = do

    [file] <- getArgs
    src <- TIO.readFile file
    case stripPos <$> runLexer src of
        Left err -> TIO.putStrLn err
        Right x -> do

            let zp = mkDgZp $ dropWhitespace x
            forM_ (zpToList zp) (print . (here,))

--             print (here, "--------------------------------------------------")

            case applyDgp test002' zp of
                Right (ast, (DgPSt _ c@(Zp zpl zpr) _ _)) -> do
--                     print (here, a, c)
                    print (here, "--------------------------------------------------")
                    for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
--                     for_ zpr $ \q -> print (here, q)

                    print (here, "--------------------------------------------------")
--                     print (here, ast)
--                     print (here)
--                     printAst 0 ast
                    TIO.putStrLn src
                    testAst ast
                    print (here, "--------------------------------------------------")



                Left err -> print (here, err)
#if 0
            forM_ x $ \(l,c) -> do
                print l
                print c
#endif
