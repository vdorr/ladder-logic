{-# LANGUAGE CPP, ViewPatterns #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Tooling where

import Data.Semigroup
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Function
import Data.Foldable
import Data.Tuple
import Data.Maybe
import Data.Bifunctor

import Data.Int
import Data.Word

import Control.Monad.Writer.Strict

-- import Ladder.Zipper
import Ladder.Lexer -- (preproc5', runLexer, dropWhitespace)
import Ladder.DiagramParser
import Ladder.LadderParser
import Language.Ladder.Utils

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
    | AndN n
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

vect01 :: TestVect
vect01 =
    [ (2, [("a", X False),("b", X False),("c", X False),("d", X False)])
    , (1, [("a", X True)])
    , (1, [("a", X False)])
    ]

--------------------------------------------------------------------------------

-- http://hackage.haskell.org/package/bits
-- http://hackage.haskell.org/package/haskell-modbus
-- https://github.com/yaacov/ArduinoModbusSlave/blob/master/examples/full/full.ino

 --wire stack count, wire stack, arg stack, memory???
type ItpSt2 = (Word8, Word16, [Int16], ([Word8], [Int16]))

eval2 :: ItpSt2 -> Instruction Int Int16 -> Either (ItpSt2, String) ItpSt2
eval2 = undefined

run2
    :: ItpSt2
    -> [(String, [ExtendedInstruction String Int Int16])]
    -> Either (ItpSt2, String) ItpSt2
run2 = undefined

data ExtendedInstruction ca a w
    = EIJump ca
    | EISimple (Instruction a w)
    deriving Show

data Instruction a w
    = ITrap --invoke debugger
    | ISysRq
    | ILdOn -- push #1 onto wire stack {- w: -- #1 -}
    | IDup -- coudl be replaced by IPick 0, dup value on top of wire stack {- w: x -- x x -}
    | IPick  Int -- push wire stack value at index onto wire stack {- w: -- x -}
    | IDrop --drop value from wire stack {- w: x -- -}

    | ILdBit a -- push bit from address onto wire stack
    | IStBit a -- dtto for store

--     | IJump  String
    | IAnd -- and two values on wire stack, push result back
    | IOr -- dtto, or
    | INot -- negate value on top of wire stack
--     | IXor

    | ILdCnA w {- push int const onto argument stack, a: -- l -}
    | ILdM {- a: size addr -- <value> -}
    | IStM

--     | IOp Operator --instead of lt,gt,etc
    | IEq -- compare two value on arg stack and push result onto wire stack
    | ILt
    | IGt
    deriving Show

-- data ItSt = ItSt [Bool] [V] [(String, V)]
type ItpSt = ([Bool], [V], [(String, V)])

eval :: ItpSt -> Instruction String Int -> Either (ItpSt, String) ItpSt
eval = f
    where
    f st                 ITrap      = Left (st, "trap")
    f    (  ws, os, m)   ILdOn      = pure (True:ws, os, m)
    f    (w:ws, os, m)   IDup       = pure (w:w:ws, os, m)
    f st@(  ws, os, m)  (IPick i)
        | i >= 0 && i < length ws   = pure (ws!!i:ws, os, m)
        | otherwise                 = Left (st, "stk idx out of range")
    f    (_:ws, os, m)   IDrop      = pure (ws, os, m)
    f st@(ws,   os, m)  (ILdBit a)
        | Just (X v) <- lookup a m  = pure (v:ws, os, m)
        | otherwise                 = Left (st, "invalid memory access")
    f st@(w:ws, os, m)  (IStBit a)
        | (m0,(_,X _):m1) <- break ((==a).fst) m
                                    = pure (w:ws, os, (m0 ++ (a, X w) : m1))
        | otherwise                 = Left (st, "invalid memory access")
    f st@(a:b:ws, os, m) IAnd       = pure ((a&&b):ws, os, m)
    f st@(a:b:ws, os, m) IOr        = pure ((a||b):ws, os, m)
    f st@(a:ws,   os, m) INot       = pure (not a:ws,  os, m)

--     f _ i = error $ show (here, i)

--     f    (ItSt ws     os         m) (ILdArg o) = pure $ ItSt ws (o:os) m
--     f st@(ItSt ws     (Var n:os) m)  ILdM
--         | Just v <- lookup n m                 = undefined --pure $ ItSt ws os m
--         | otherwise                            = Left (st, "var not found")

--------------------------------------------------------------------------------

-- |Return list of annotations (usually positions) of 'Sink' nodes
stubs
    :: Cofree (Diagram c d s) p
    -> [p]
stubs (p :< a) = f a
    where
    f (Source a)   = stubs a --ugh what is ungawa for this?
    f  Sink        = [p]
    f  End         = []
    f (Device d a) = stubs a
    f (Jump s)     = []
    f (Node a)     = foldMap stubs a
    f (Conn c    ) = [] --XXX i am not sure !!!
    f (Cont c   a) = []

--just remove initial Source and tree of Nodes
--quite useless operation, i think
forest
    :: Cofree (Diagram c d s) p
    -> Maybe [Cofree (Diagram c d s) p]
-- forest (p :< Source a) = Just $ fmap ((p :<) . Source) $ fst $ succs' a
forest (_ :< Source a) = Just $ fmap (\n@(p :< _) -> p :< Source n) $ fst $ succs' a
forest _               = Nothing

merge'
    :: Cofree (Diagram c d s) p -- ^input tree
    -> ([(p, [p])], Cofree (Diagram c d s) p)
-- ^pairs of position of 'Node' still in tree and 'Node' positions merged into it and new tree
merge' = mapAccumL (\ns (nss, p) -> (f ns p nss, p)) [] . merge
-- merge' ast = mapAccumL (\ns (nss, p) -> (ns ++ nss, p)) [] $ merge ast
    where
    f ns _ []  = ns
    f ns p nss = (p, nss) : ns

merge
    :: Cofree (Diagram c d s) p
    -> Cofree (Diagram c d s) ([p], p)
merge = g
    where
    g (p :< Node as) = (ns, p) :< Node (fmap merge as')
        where
        (as', ns) = foldMap succs' as
    g (p :< other)   = ([], p) :< fmap merge other

-- succs
--     :: Cofree (Diagram d s) p
--     -> [Cofree (Diagram d s) p]
-- succs (_ :< Node xs) = foldMap succs xs
-- succs other          = [other]

succs'
    :: Cofree (Diagram c d s) p
    -> ([Cofree (Diagram c d s) p], [p])
succs' (p :< Node xs) = fmap (++[p]) $ foldMap succs' xs
-- succs' (p :< Node xs) = foldMap succs' xs
succs' other          = ([other], [])

--------------------------------------------------------------------------------

--XXX XXX XXX prob want new base functor, without 'End' (and without 'Source'?)
-- and distinguishing 'Sink' and 'Stub'

--only node may have multiple successors
--stub(sink) may have one or zero successors
--i should make newtype for nodeToSink elements

--TODO allow ading annotations in 'emit'

generate ::
    Monad m0 =>
    Show b0 =>
    Show s1 =>
    Show s0 =>
    Eq b0 =>
    Eq c =>
    Show c =>
    ([Instruction String w] -> m0 ())
                      -> [(b0, b0)]
                      -> [([b0], Cofree (Diagram c (Op Operand s0) s1) b0)]
                      -> m0 ([b0], [([b0], Cofree (Diagram c (Op Operand s0) s1) b0)])

generate emit nodeToSink asts = go ([], asts)

    where

    sinkToNode = nub $ fmap swap nodeToSink --do i need nub? i think yes

    go (stack, []) = do
        return (stack, [])
--    go stack (p :< a) xs -- get rid of stubs
    go (stack, (stubs, p :< a) : xs) = f stack a

        where

        f stk (Source b)       = do
            emit [ILdOn]
            go (p:stk, (stubs, b) : xs)
        f (_:stk) Sink         = do
            case lookup p sinkToNode of --XXX here is argument for distinguishing 'Sink' and 'Stub'
                Just _  -> return (p:stk, xs) --put value back under name with which is referenced
                Nothing -> do
                    emit [IDrop]
                    return (stk, xs)
        f stk End              = go (stk, xs)
        f (x:stk) (Device d b) = do
            case d of
                 And (Var addr) -> emit [ILdBit addr, IAnd]
                 St (Var addr)  -> emit [IStBit addr]
            go (p:stk, (stubs, b):xs)
        f stk (Jump s)         = error here --later
        f (_:stk) (Node b)     = do
            --at least one value on stack
            --look for stubs coinciding with this node
            let needToEvalFirst1{-node positions-}
                    = filter ((p==).fst) nodeToSink

            (stk', xs') <-
                foldlM
                    step
                    (stk, xs)
                    needToEvalFirst1

            let dups = replicate (length b - 1) p
--             liftIO $ print (here, ">>>>>>>>>", length b)
            for_ dups $ const $ emit [IDup]

            foldlM
                (\(stk'', xs'') tr
                    -> go (stk'', ([{-don't care about stub list here-}], tr) : xs'')
                    )
                (dups ++ stk', xs')
                b
            where
            step (stk', xs') (_, stubP) = do
                case findIndex (stubP==) stk' of
                     Just i -> do
                         case i of
                            0 -> return ()
                            _ -> emit [IPick i]
                         emit [IOr]
                         return (stubP:stk', xs')
                     Nothing ->
                        case partition (elem stubP .fst) xs' of
                          --stub must appear in exactly one subtree
                            ([tr@(_, _:< _)], rest) -> do
                                s@(stk'', _) <- go (stk', tr : rest)
                                --now fetch and or??
                                case findIndex (stubP==) stk'' of --check latest stack
                                    Just i ->
                                        case i of
                                            0 -> return ()
                                            _ -> emit [IPick i]
                                    Nothing
                                        -> error $ show (here, stubP, stk'') --should not happen
                                emit [IOr]
                                return s
                            other -> error $ show (here, other) --should not happen

--         f (pp:stk) (Conn c) = do
--             return (c:stk, xs)
--         f stk (Cont stubP b) = do
--             case findIndex (stubP==) stk of
--                 Just i -> do
--                     case i of
--                         0 -> return ()
--                         _ -> emit [IPick i]
-- --                     return (stubP:stk, xs)
--                     go (stubP:stk, (stubs, b) : xs)
-- 
--                 Nothing -> error here
-- --             undefined

        f stk n = error $ show (here, stk, n)

--------------------------------------------------------------------------------

ldlines'' :: [Cofree (Diagram () d s) DgExt] -> [Cofree (Diagram DgExt d s) DgExt]
-- ldlines'' = sortOn (\(p:<_)->p) . foldMap ldlines'
ldlines'' = foldMap ldlines'

ldlines' :: Cofree (Diagram () d s) DgExt -> [Cofree (Diagram DgExt d s) DgExt]
-- ldlines' tr = let (a, b) = ldlines tr in a : b
ldlines' = toList . ldlines

ldlines
    :: Cofree (Diagram () d s) DgExt
--NonEMpty seem like better option here, foldabale etc
--     -> ( Cofree (Diagram DgExt d s) DgExt
--        , [Cofree (Diagram DgExt d s) DgExt]
--        )
    -> NonEmpty (Cofree (Diagram DgExt d s) DgExt)
ldlines tr = let (a, b) = f tr in a :| b
    where

    f (p@(ln, _) :< Node l) = (p :< Node (a' ++ fmap (const (p :< Conn p)) b) , as'')
        where

        as'' = concat as' ++ fmap (\nn@(pp:<_) -> pp :< Cont p nn) b' ++ concat bs'

        (a, b) = partition (\((ln', _) :< _) -> ln'==ln) l
        (a', as') = unzip $ fmap f a
        (b', bs') = unzip $ fmap f b
    f (p :< Source a)   = bimap ((p:<) . Source) id $ f a
    f (p :< Sink)       = (p :< Sink, [])
    f (p :< End)        = (p :< End, [])
    f (p :< Device d a) = bimap ((p:<) . Device d) id $ f a
    f (p :< Jump s)     = (p :< Jump s, [])

--------------------------------------------------------------------------------

nodeTable :: [(p, [p])] -> [(p, p)]
nodeTable = foldMap (\(x, xs) -> (x, x) : fmap (,x) xs)

generateStk :: Cofree (Diagram () Dev String) DgExt -> IO [Instruction String Int]
generateStk ast' = do
    let ast = parseOps ast'

    print (here, "-----------------------")

    --chop
    let Just (x1_0 ::[Cofree (Diagram () (Op Operand String) String) DgExt])
            = forest ast
--     let x1_x = ldlines'' [ast]
--     let x1_xxx = ldlines'' x1_0
-- 
--     for_ x1_x print
--     print (here, "-----------------------")
--     let x1 = x1_x
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

--------------------------------------------------------------------------------

evalBlock :: [Instruction String Int] -> ItpSt -> Either (ItpSt, String) ItpSt
evalBlock p st = foldlM eval st p

evalTestVect''
    :: [Instruction String Int]
    -> [VarName]
    -> [(Int, [(VarName, V)])]
    -> Either (ItpSt, String) [[V]]
evalTestVect'' prog watch vect = fst <$> foldlM step ([], ([],[],[])) vect'
    where

    vect' = flattenTestVect vect

    step (tr, st@(w, o, mem)) stim = do
        st'@(_, _, mem'') <- evalBlock prog (w, o, mem')
        let tr' = [ v | (flip lookup mem'' -> Just v) <- watch ]
        return (tr ++ [tr'], st')
        where
        mem' = updateMemory mem stim

--------------------------------------------------------------------------------

parseOps
    :: Cofree (Diagram c Dev s) p
    -> Cofree (Diagram c (Op Operand s) s) p
parseOps (a :< n) = a :< fmap parseOps (mapDg id f id n)
    where
    f (Dev "[ ]" [n]   ) = And  n
    f (Dev "[/]" [n]   ) = AndN  n
    f (Dev "[>]" [a, b]) = Cmp Gt a b
    f (Dev "( )" [n]   ) = St n
    f _                  = error here
