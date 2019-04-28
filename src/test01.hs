{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances,
    PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable,
    LambdaCase, ScopedTypeVariables, ViewPatterns #-}

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

colRight :: DgExt -> DgExt
colRight (ln, (_, co)) = (ln, (co + 1, co + 1))

colUnder :: DgExt -> DgExt
colUnder (ln, (_, co)) = (ln + 1, (co, co))

fff (p :< Source a) = do
    ff [] 0 p a
fff _ = fail here

ff st r src (p :< x) = do
    case x of
--         Source a -> do
--             print (here, "Source", src)
--             ff p a
        Source a -> undefined

        Sink -> do
            print (here, "Sink", r, ">>", (colRight p), lookup (colRight p) st)
            return $ st ++ [((colRight p), ("Sink", r))]

        End -> do
            print (here, "End", r, p, lookup (colUnder p) st)
            return $ st ++ [((colUnder p), ("End ", r))]

        Device s [n] a -> do
            print (here, "Device", n, r)
            ff st (r+1) p a

        Jump s -> do
            print (here, "Jump", r)
            return st
        Label s a -> undefined
        Node la -> do
            print (here, "Node", r, ">>", p)
            doNode (st ++ [(p, ("node", r))]) la
    where
    doNode st' [] = return st'
    doNode st' (x' : xs) = do
        st'' <- ff st' (r+1) p x'
        doNode st'' xs

testAst ast = do
    print (here, ast)
--     ff (-1,(-1,-1)) ast
    w <- (reverse . nub) <$> fff ast
    print (here, "-----------------------")
    for_ w print
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
                Right (ast, (DgPSt _ c@(Zp zpl zpr) _)) -> do
--                     print (here, a, c)
                    print (here, "--------------------------------------------------")
                    for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
--                     for_ zpr $ \q -> print (here, q)

                    print (here, "--------------------------------------------------")
                    testAst ast
                    print (here, "--------------------------------------------------")



                Left err -> print (here, err)
#if 0
            forM_ x $ \(l,c) -> do
                print l
                print c
#endif
