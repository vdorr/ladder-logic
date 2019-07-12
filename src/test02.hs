{-# OPTIONS_GHC -Wunused-imports -Wall #-}
{-# LANGUAGE CPP #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import TestUtils

--------------------------------------------------------------------------------

main :: IO ()
main = do
    [file] <- getArgs
    src <- TIO.readFile file
    (tst, lxs) <- loadLadderTest file
    case tst of
        Just test -> do
            print (here, test)
            TIO.putStrLn src
            ast <- parseOrDie2 lxs
            passed <- runLadderTest True test ast
            print (here, passed, if passed then "PASSED" else "FAILED")
        Nothing -> print (here, "no embedded test found")
