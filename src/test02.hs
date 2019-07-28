{-# OPTIONS_GHC -Wunused-imports -Wall #-}
{-# LANGUAGE CPP #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Language.Ladder.Interpreter --FIXME
import Language.Ladder.Simple --FIXME

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
            ast <- parseOrDie2 (wrapDevice3 (pure . I) (pure . A)) lxs
            passed <- runLadderTest2 False test ast
            print (here, passed, if passed then "PASSED" else "FAILED")
        Nothing -> print (here, "no embedded test found")
