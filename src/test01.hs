{-# OPTIONS_GHC -Wunused-imports #-}

{-# LANGUAGE CPP, TupleSections, TypeSynonymInstances, FlexibleInstances,
    QuasiQuotes, PatternSynonyms,TypeApplications,DeriveAnyClass,
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
import Data.Bifunctor
import System.Environment (getArgs)
import Data.Tuple
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

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

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
dropEnd (p :< a) = p :< go a
    where
    go (Node ns) = Node (fmap dropEnd (filter notEnd ns))
    go n         = fmap dropEnd n
    notEnd (_ :< End) = False
    notEnd _          = True

--assuming spatially sorted input (by lines then by columns)
tsort2
    :: [(DgExt, [DgExt])]
    -> [Cofree (Diagram DgExt d s) DgExt]
    -> [Cofree (Diagram DgExt d s) DgExt]
tsort2 mergedNodes asts = f asts'
    where
    asts' = fmap (\n -> (dependencies' n, n)) asts

    dependencies' n = deps { nodes = nodes'}
        where
        deps = dependencies n
        nodes' = foldMap allNodeLocs (nodes deps)

    --assuming p is in list in mergedNodes
    allNodeLocs p = fromMaybe [p] $ lookup p mergedNodes

    f ((d, x) : xs) = fmap snd a ++ [x] ++ f b
        where
        (a, b) = partition (dependsOn d . fst) xs
    f [] = []

dependsOn :: Deps DgExt -> Deps DgExt -> Bool
a `dependsOn` b = sinks b `someIsIn` nodes a
                    || conns b `someIsIn` conts a
    where
    someIsIn x y = any (flip elem y) x

data Deps p = Deps { nodes, sinks, conts, conns :: [p] }
    deriving (Show, Semigroup, Monoid)

dependencies :: Cofree (Diagram DgExt d s) DgExt -> Deps DgExt
dependencies = cata' go
    where
    go (p, n) = f n
        where
        f (Source   a ) = a
        f  Sink         = Deps [] [p] [] []
        f  End          = mempty
        f (Device _ a ) = a
        f (Jump _     ) = mempty
        f (Node     as) = Deps [p] [] [] [] <> mconcat as
        f (Conn c     ) = Deps [] [] [] [p]
        f (Cont c   a ) = Deps [] [] [p] [] <> a

unFix' :: Cofree f a -> (a, f (Cofree f a))
unFix' (a :< f) = (a, f)

cata' :: Functor f => ((w, f a) -> a) -> Cofree f w -> a
cata' alg = alg . fmap (fmap (cata' alg)) . unFix'

-- cut'
--     :: [Cofree (Diagram DgExt d s) DgExt]
--     -> [Cofree (Diagram DgExt d s) DgExt]
-- cut' = foldMap cut''
-- 
-- cut''
--     :: Cofree (Diagram DgExt d s) DgExt
--     -> [Cofree (Diagram DgExt d s) DgExt]
-- cut'' = uncurry (:) . cut

--------------------------------------------------------------------------------

-- cut
--     :: Cofree (Diagram DgExt d s) DgExt
--     -> ( Cofree (Diagram DgExt d s) DgExt
--        , [Cofree (Diagram DgExt d s) DgExt])
-- cut (p :< a) = f a
--     where
--     f (Source   a) = h Source (cut a)
--     f  Sink        = (p :< Sink, [])
--     f  End         = (p :< End, [])
--     f (Device d a) = h (Device d) (cut a)
--     f (Jump s    ) = (p :< Jump s, [])
-- 
--     f (Node     a) = (p :< Node [p :< Conn p], x' ++ concat y)
--         where
--         (x, y) = unzip $ fmap cut a
--         x' = (fmap (\n@(pp:<_) -> pp:<Cont p n) x)
-- 
--     f (Conn c    ) = (p :< Conn c, [])
--     f (Cont c   a) = h (Cont c) (cut a)
-- 
--     h g w = bimap ((p :<) . g) id w


--TODO TEST every list elemen has all nodes on same line
cut1' = foldMap (uncurry (:) . cut1)

sameLine :: Cofree (Diagram c d s) DgExt -> Bool
sameLine n@((ln, _) :< _) = getAll $ foldMap (All.(ln==).fst) n

--TODO ensure cut when line number changes
cut1
    :: Cofree (Diagram () d s) DgExt
    -> ( Cofree (Diagram DgExt d s) DgExt
       , [Cofree (Diagram DgExt d s) DgExt])
cut1 (p :< a) = f a
    where
--     f :: Diagram () d1 s1 (Cofree (Diagram () d1 s1) DgExt)
--                       -> (Cofree (Diagram DgExt d1 s1) DgExt,
--                           [Cofree (Diagram DgExt d1 s1) DgExt])
    f (Source   a) = h Source (cut1 a)
    f  Sink        = (p :< Sink, [])
    f  End         = (p :< End, [])
    f (Device d a) = h (Device d) (cut1 a)
    f (Jump s    ) = (p :< Jump s, [])

    f (Node     a) = (p :< Node [p :< Conn p], x' ++ concat y)
        where
        (x, y) = unzip $ fmap cut1 a
        x' = (fmap (\n@(pp:<_) -> pp :< Cont p n) x)

    f (Conn _    ) = error here
    f (Cont _   _) = error here

    h g w = bimap ((p :<) . g) id w

--------------------------------------------------------------------------------

-- cutcut
--     :: Cofree (Diagram () d s) DgExt
--     -> NonEmpty (Cofree (Diagram DgExt d s) DgExt)
-- cutcut tr = let (a, b) = f tr in a :| b
--     where
-- 
--     f (p@(ln, _) :< Node l) = (p :< Node (a' ++ fmap (const (p :< Conn p)) b) , as'')
--         where
-- 
--         as'' = concat as' ++ fmap (\nn@(pp:<_) -> pp :< Cont p nn) b' ++ concat bs'
-- 
--         (a, b) = partition (lineNumberIs ln) l
--         (a', as') = unzip $ fmap f a
--         (b', bs') = unzip $ fmap f b
--     f (p :< Source a)   = bimap ((p:<) . Source) id $ f a
--     f (p :< Sink)       = (p :< Sink, [])
--     f (p :< End)        = (p :< End, [])
--     f (p :< Device d a) = bimap ((p:<) . Device d) id $ f a
--     f (p :< Jump s)     = (p :< Jump s, [])
-- 
--     lineNumberIs ln'' = \((ln', _) :< _) -> ln'==ln''

position (p :< _) = p

--------------------------------------------------------------------------------

generate2 ::
    Monad m0 =>
    Show b0 =>
    Show s1 =>
    Show s0 =>
    Eq b0 =>
    Eq c =>
    Show c
    => ([Instruction String w] -> m0 ())
    -> [Cofree (Diagram c (Op Operand s0) s1) b0]
                      -> Cofree (Diagram c (Op Operand s0) s1) b0
                      -> m0 [Cofree (Diagram c (Op Operand s0) s1) b0]

generate2 emit stk0 asts = go stk0 asts

    where

    go stack nd@(p :< a) = f stack a
        where

        f stk (Source b)       = do
            emit [ILdOn]
            go (nd:stk) b
        f (_:stk) Sink         = do
            return (nd:stk)
--             case lookup p sinkToNode of --XXX here is argument for distinguishing 'Sink' and 'Stub'
--                 Just _  -> return (nd:stk) --put value back under name with which is referenced
--                 Nothing -> do
--                     emit [IDrop]
--                     return (stk)
        f stk End              = return (stk) --XXX emit Drop?
        f (x:stk) (Device d b) = do
            emitDevice d
            go (nd:stk) b
        f stk (Jump s)         = error here --later
        f (_:stk) (Node b)     = do

--i think dups should be emitted only AFTER node value is computed

    --fold over stack, picking sinks(aka stubs), emitting Or's
            --depth of stack stays the same during evaluation of preds
            for_ (zip stk [0..]) $ \(x, i) -> do
                case x of
                     pp :< Sink | pp == p -> do
                         bringToTop i
                         emit [IOr]
                     _ -> return ()

--             let dups = replicate (length b - 1) nd
--             for_ dups $ const $ emit [IDup]
            let copiesOnStack = fmap (const nd) b
            replicateM (length b - 1) $ emit [IDup]

--             liftIO $ print (here)
            foldlM
                (go
    --emit Dup's here? --so that stack level is kept low
                )
--                 ([nd] ++ dups ++ stk)
                (copiesOnStack ++ stk)
                b
        f (pp:stk) (Conn c) = do
--             liftIO $ print here
            return (nd:stk)
        f stk (Cont stubP b) = do
--             liftIO $ print here
            case findIndex (isConn stubP) stk of
                Just i -> bringToTop i
                Nothing -> error here --should not happen
            go (nd:stk) b --XXX not sure about nd on stack here

        f stk n = error $ show (here, stk, n)

    isConn p0 (_ :< Conn p1) = p0 == p1
    isConn _ _ = False

    bringToTop 0 = return ()
    bringToTop i = emit [IPick i]

    emitDevice d =
        case d of
                And  (Var addr) -> emit [ILdBit addr, IAnd]
                AndN (Var addr) -> emit [ILdBit addr, INot, IAnd]
                St   (Var addr) -> emit [IStBit addr]
                _               -> error $ show (here, d)

--------------------------------------------------------------------------------

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

--     let a2 = ldlines'' a1
--     let a3 = cut' a2

--     let a2 = ldlines'' a1
--     let a3 = cut' a2
--     let a4 = sortOn (\(p:<_) -> p) a3

    let a5 = cut1' a1
    let a6 = sortOn position a5

    for_ a6 print
    print (here, "-----------------------")

    q :: [Instruction String Int] <- execWriterT $ foldlM (generate2 tell) [] a6
    for_ q print
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
