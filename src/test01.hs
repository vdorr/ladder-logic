{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances,
    PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable,
    LambdaCase, ScopedTypeVariables, ViewPatterns #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import Data.Foldable
import Data.Traversable
import System.Environment (getArgs)

import Preprocess
import Tokenizer (preproc5')
import Zipper

import LadderParser

--------------------------------------------------------------------------------

#if 0
data V s = VTrue | VVar s | VOp Int s

--list of operators, their position, IDs and or-ed
-- type St k = [(k, (Int, [String]))]

--list of registers, or-ed list of source values
type St k = [(k, ())]
-- register number <- or [sources...]

--lookup postion, retrive reg number, add op to or list
register = undefined

--input of operator is its input wire and zero or more explicit values

ff f (a :< x) = f a (fmap (??) x)

g st src k (Source a) = g 0 a ... put st k VOn
    register st k VTrue

g st src k (Device s [s] a
    register st k (VOp src)

g st src k (Node [a]
    register st k VOn

g st src k (Jump s
g st src k  Sink --end of hline, lookup node at same (end of extent) position
g st src k  End --end of vline, lookup node at same (end of extent) position

g _  _   _ (Label s a) = undefined

#endif

-- Source a
-- Sink
-- End
-- Device s ls a
-- Jump s
-- Label s a
-- Node la

ff src (p :< x) = do
    case x of
        Source a -> do
            print (here, "Source", src)
            ff p a
        Sink -> do
            print (here, "Sink", src)
        End -> print (here, "End", src)
        Device _s [n] a -> do
            print (here, "Device", n, src)
            ff p a
        Jump s -> do
            print (here, "Jump", src)
        Label s a -> undefined
        Node la -> do
            print (here, "Node", src)
            for_ la (ff p)

testAst ast = do
    print (here, ast)
    ff (-1,(-1,-1)) ast
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
