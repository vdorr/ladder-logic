{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances,
    PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable,
    LambdaCase, ScopedTypeVariables, ViewPatterns #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import Data.Foldable
import System.Environment (getArgs)

import Preprocess
import Zipper

--------------------------------------------------------------------------------

main = do
    [file] <- getArgs
    src <- TIO.readFile file
    case preproc4'' src of
        Left err -> TIO.putStrLn err
        Right x -> do
--             print $ stripPos x
            let zp@(Zp zpl zpr) = mkDgZp x

--             print (here, zp)
            for_ zpl $ \q -> print (here, q)
            for_ zpr $ \q -> print (here, q)

            print (here, "--------------------------------------------------")
            case applyDgp test001 zp of
                Right (_, (DgPSt _ c@(Zp zpl zpr) _)) -> do
--                     print (here, a, c)
                    for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
--                     for_ zpr $ \q -> print (here, q)
                Left err -> print (here, err)

            print (here, "--------------------------------------------------")

            case applyDgp test002 zp of
                Right (_, (DgPSt _ c@(Zp zpl zpr) _)) -> do
--                     print (here, a, c)
                    for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
--                     for_ zpr $ \q -> print (here, q)
                Left err -> print (here, err)
#if 0
            forM_ x $ \(l,c) -> do
                print l
                print c
#endif
