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

import LadderParser

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
    | M String
    | DD -- dummy
    deriving (Show, Eq, Ord)

-- data E = Op String [E] | Data D | C Bool
--     deriving Show

data E = Op String [D] -- and, or, ld #on, ...
    deriving Show

fffff (p :< Source a) =  ffff ([], [(R 0, Op "ld #on" [])], 1) 0 p a
fffff _ = error here

ffff (st, op, cnt) r src (p :< x) = f x
    where
    f (Source a) = undefined --should not happen
    f (Label s a) = undefined --should not happen

    f  Sink = --end of hline, may lead to 'Node'
--         print (here, "Sink", r, ">>", p, lookup p st)

--         ( st ++ [(p, ("Sink", r))]
        ( st
        , op ++ case lookup p st of
                     Nothing -> []
                     Just rr -> [ (R rr, Op "ld" [R r]) ]
        , cnt
        )

    f  End = --end of vertical line
--should really appear only once at end of left power rail
--should test this (exactly one End in rung)
--         print (here, "End", r, p, lookup p st)
--         (st, op)
        ( st
        , op 
--             ++ case lookup p st of
--                      Nothing -> []
--                      Just rr -> [ "$" ++ show rr ++ " |= $" ++ show r ++ ";" ]
        , cnt
        )

    f (Device s n a) =
--         print (here, "Device", n, r)
        ffff
            (st
            , op ++ getop r cnt s n
            , cnt + 1) (cnt) p a


    f (Jump s) =
--         print (here, "Jump", r)
        (st, op ++ [(DD, Op ("jump to " ++ s) [])], cnt) --XXX XXX beware wires crossing jump point
    f (Node la) =
--         print (here, "Node", r, ">>", p)
--         doNode (st ++ [(p, ("node", r))], op) la
        doNode (st ++ [(p, r)], op, cnt) la

    doNode st' [] = st'
    doNode st' (x' : xs) =
        let st'' = ffff st' r p x'
            in doNode st'' xs

    getop rr rrr "[ ]" [n] = [(R rrr, Op "and" [ R rr, M n])]
    getop rr rrr "( )" [n]
        = [(M n, Op "ld" [R rr]), (R rrr, Op "ld" [R rr])]
    getop rr _ s n = error $ show (here, s, n)

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

-- FIXME later tsort :: [(D, [a])] -> Maybe [(D, [a])]
tsort :: [Int] -> [(D, [E])] -> [(D, [E])]
tsort _ [] = []
tsort ks xs = yes ++ tsort (ks ++ concatMap (getRegN . fst) yes) no
    where
    (yes, no) = partition (all test . snd) xs

    test (Op _ a) = all isIn a

    isIn (R n) = elem n ks
    isIn _     = True

    getRegN (R n) = [n]
    getRegN _     = []

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
    for_ (tsort [] $ or'd [] op) print
    print (here)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    [file] <- getArgs
    src <- TIO.readFile file
    case stripPos <$> preproc5' src of
        Left err -> TIO.putStrLn err
        Right x -> do
--             print $ stripPos x
            let zp@(Zp zpl zpr) = mkDgZp x

--             print (here, zp)
--             for_ zpl $ \q -> print (here, q)
--             for_ zpr $ \q -> print (here, q)
            forM_ (reverse zpl ++ zpr) $ \q -> print (here, q)

--             print (here, "--------------------------------------------------")
--             case applyDgp test001 zp of
--                 Right (_, (DgPSt _ c@(Zp zpl zpr) _)) -> do
-- --                     print (here, a, c)
--                     for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
-- --                     for_ zpr $ \q -> print (here, q)
--                 Left err -> print (here, err)

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
