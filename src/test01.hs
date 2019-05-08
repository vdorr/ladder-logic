{-# LANGUAGE CPP, TupleSections, TypeSynonymInstances, FlexibleInstances,
    PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable,
    LambdaCase, ScopedTypeVariables, ViewPatterns #-}

-- OverloadedStrings, 

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import Data.Foldable
import Data.Traversable
import Data.List
import System.Environment (getArgs)

import Preprocess
import Tokenizer (preproc5')
import Zipper

import Ladder.LadderParser

--------------------------------------------------------------------------------

-- data V s = VTrue | VVar s | VOp Int s [s]
--     deriving (Show, Eq)

-- Source a
-- Sink
-- End
-- Device s ls a
-- Jump s
-- Label s a
-- Node la

--------------------------------------------------------------------------------

-- ffff (st, op) r src (p :< x) = f x
--     where
--     f (Source a) = undefined --should not happen
--     f (Label s a) = undefined --should not happen
-- 
--     f  Sink = do --end of hline, may lead to 'Node'
--         print (here, "Sink", r, ">>", p, lookup p st)
--         return (st ++ [(p, ("Sink", r))], op)
-- 
--     f  End = do --end of vertical line
-- --should really appear only once at end of left power rail
-- --should test this (exactly one End in rung)
--         print (here, "End", r, p, lookup p st)
--         return (st, op)
-- 
--     f (Device s [n] a) = do
--         print (here, "Device", n, r)
--         ffff (st, op) (r+1) p a
-- 
--     f (Jump s) = do
--         print (here, "Jump", r)
--         return (st, op)
--     f (Node la) = do
--         print (here, "Node", r, ">>", p)
--         doNode (st ++ [(p, ("node", r))], op) la
-- 
--     doNode st' [] = return st'
--     doNode st' (x' : xs) = do
--         st'' <- ffff st' r p x'
--         doNode st'' xs

--------------------------------------------------------------------------------

data D
    = R Int
    | DD -- dummy
    deriving (Show, Eq, Ord)

data E op = Op op [D] -- and, or, ld #on, ...
    deriving Show

data V
    = X Bool 
    | I Int
    deriving (Show, Eq)

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

--TODO
--TODO
--TODO

--convert to some intermediate representation usable outside interpreter
--that is - extend with node numbering

--TODO
--TODO
--TODO

fffff
    :: Cofree (Diagram String Operand s) DgExt
    -> ([(DgExt, Int)], [(D, E (Op Operand s))], Int)

fffff (p :< Source a) =  ffff ([], [(R 0, Op On [])], 1) 0 p a
fffff _               = error here --should not happen

ffff
    :: ([(DgExt, Int)], [(D, E (Op Operand s))], Int)
    -> Int
    -> DgExt
    -> Cofree (Diagram String Operand s) DgExt
    -> ([(DgExt, Int)], [(D, E (Op Operand s))], Int)
ffff (st, op, cnt) r src (p :< x) = f x
    where
--     f (Label s a) = undefined --should not happen
    f (Source a) = error here --should not happen
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
    f (Device s n a) =
        ffff
            (st
            , op <> getop r cnt s n
            , cnt + 1) cnt p a
    f (Jump s) =
        (st, op <> [(DD, Op (Jmp s) [R r])], cnt) --XXX XXX beware wires crossing jump point
    f (Node la) =
        doNode (st <> [(p, r)], op, cnt) la

    doNode st' [] = st'
    doNode st' (x' : xs) =
        let st'' = ffff st' r p x'
            in doNode st'' xs

    getop rr rrr "[ ]" [n]    = [(R rrr, Op (And n) [ R rr ])]
    getop rr rrr "[>]" [a, b] = [(R rrr, Op (Cmp Gt a b) [ R rr ])]
    getop rr rrr "( )" [n]
        = [(DD, Op (St n) [R rr]), (R rrr, Op Ld [R rr])]
    getop rr _ s n = error $ show (here, s, n)

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
        g  DD   = (r              , m1)
        (m1, w') = foldl h (m, True) op
        h (m', w) o = fmap (w ||) (rung m' r o)


-- rung :: Eq a0 => [(a0, Bool)] -> [(Int, Bool)] -> E (Op a0) -> ([(a0, Bool)], Bool)
rung m r (Op o a) = op o a
    where
    op (And (Var c))      [R n] = (m      , reg n && ldx c)
    op (St  (Var c))      [R n] = (st y c , y)
        where y = reg n
    op  Ld          [R n] = (m      , reg n)
    op  On          []    = (m      , True)
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
or'd out ((d, x) : xs) = or'd ((d, [x]) : out) xs
or'd out []            = reverse out

--make tail recursive?
tsort :: [Int] -> [(D, [E op])] -> Maybe [(D, [E op])]
tsort _ [] = return []
tsort ks xs = do
    (yes@(_ : _), no) <- return $ partition (all test . snd) xs
    (yes ++) <$> tsort (ks ++ foldMap (getRegN . fst) yes) no
    where

    test (Op _ a) = all isIn a

    isIn (R n) = elem n ks
    isIn _     = True

    getRegN (R n) = [n]
    getRegN _     = []

--------------------------------------------------------------------------------

testAst :: Cofree (Diagram String Operand String) DgExt
                      -> IO ()
testAst ast = do
--     print (here, ast)
--     ff (-1,(-1,-1)) ast
--     w <- (reverse . nub) <$> fff ast
--     print (here, "-----------------------")
--     for_ w print
--     print (here)

    let (st, op, cnt) = fffff ast
    print (here, cnt, "-----------------------")
    for_ st print
    print (here, "-----------------------")
    for_ op print
    print (here, "-----------------------")
    Just w <- return $ tsort [] $ or'd [] op
    for_ (w) print
    print (here, "-----------------------")
    let memory =
                [ ("a", X False),("b", X False),("c", X True)
                , ("%QX0", X True)
                , ("%IX0", I 0)
                ]
    let Just p01 = tsort [] $ or'd [] op
    print (here, snd $ network p01 memory)

--------------------------------------------------------------------------------

vect01 :: TestVect
vect01 =
    [ (2, [("a", X False),("b", X False),("c", X False)])
    , (2, [("a", X True)])
    , (2, [])
    ]

flattenTestVect :: TestVect -> [[(VarName, V)]]
flattenTestVect [] = []
flattenTestVect ((d, v) : xs)
    | d >= 1    = [v] ++ replicate (d - 1) [] ++ flattenTestVect xs
    | otherwise = flattenTestVect xs

updateMemory :: [(VarName, V)] -> [(VarName, V)] -> [(VarName, V)]
updateMemory old new = undefined

type TestVect = [(Int, [(VarName, V)])]
type VarName = String
evalTestVect
    :: [(D, [E (Op Operand String)])] -- ^network to evaluate
    -> [VarName]                      -- ^watched memory variables
    -> [(Int, [(VarName, V)])]        -- ^test vector (duration, stimuli)
    -> [[V]]       -- ^resulting trace, elems are same length as watch list
evalTestVect net watch vect = fst $ foldl f ([], []) vect'
    where
    p = snd . network net
    vect' = flattenTestVect vect

    f (tr, mem) stim = (tr ++ [tr'], mem')
        where
        mem' = p $ updateMemory mem stim
        tr' = [ v | (flip lookup mem' -> Just v) <- watch ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
    [file] <- getArgs
    src <- TIO.readFile file
    case stripPos <$> preproc5' src of
        Left err -> TIO.putStrLn err
        Right x -> do
--             print $ stripPos x
            let zp = mkDgZp x

--             print (here, zp)
--             for_ zpl $ \q -> print (here, q)
--             for_ zpr $ \q -> print (here, q)
            forM_ (zpToList zp) (print . (here,))

            print (here, "--------------------------------------------------")

            case applyDgp test002' zp of
                Right (ast, (DgPSt _ c@(Zp zpl zpr) _ _)) -> do
--                     print (here, a, c)
                    print (here, "--------------------------------------------------")
                    for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
--                     for_ zpr $ \q -> print (here, q)

                    print (here, "--------------------------------------------------")
                    print (here, ast)
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
