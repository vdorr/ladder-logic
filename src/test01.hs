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

data D = R Int | M String
    deriving Show

-- data E = Op String [E] | Data D | C Bool
--     deriving Show

data E = Op String [D] -- and, or, ld #true, ...
    deriving Show

fffff (p :< Source a) =  ffff ([], ["$0 = #on"], 1) 0 p a
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
                     Just rr -> [ "$" ++ show rr ++ " |= $" ++ show r ]
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
        (st, op ++ ["jump to " ++ s], cnt)
    f (Node la) =
--         print (here, "Node", r, ">>", p)
--         doNode (st ++ [(p, ("node", r))], op) la
        doNode (st ++ [(p, r)], op, cnt) la

    doNode st' [] = st'
    doNode st' (x' : xs) =
        let st'' = ffff st' r p x'
            in doNode st'' xs

    getop rr rrr "[ ]" [n] = ["$" ++ show rrr ++ " = $" ++ show rr ++ " and " ++ n]
    getop rr rrr "( )" [n]
        = [n ++ " = $" ++ show rr, "$" ++ show rrr ++ " = $" ++ show rr]
    getop rr _ s n = error $ show (here, s, n)

--------------------------------------------------------------------------------

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
    print (here)

--------------------------------------------------------------------------------

-- data Q a = Q String a
-- instance Show a => Show (Q a) where
--     show (Q i a) = i ++ "\n" ++ show a

-- printAst i (a :< f) = do

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
