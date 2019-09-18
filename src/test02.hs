{-# OPTIONS_GHC -Wunused-imports -Wall #-}
{-# LANGUAGE CPP #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Bifunctor
import Text.Read

import Language.Ladder.Interpreter --FIXME
import Language.Ladder.Simple --FIXME

import TestUtils
import Language.Ladder.LadderParser
import Language.Ladder.Utils

--------------------------------------------------------------------------------

main :: IO ()
main = do
    file : otherArgs <- getArgs
    src <- TIO.readFile file
    (tst, lxs) <- loadLadderTest file
    TIO.putStrLn src
    ast <- either fail pure $ parseOrDie2 (wrapDevice3 (pure . I) (pure . A)) lxs
    case (tst, otherArgs) of
        (Just test, []) -> do
            print (here, test)
--             let ast = fmap (second (mapDg id snd id)) ast'
--             let ast = fmap (second (mp)) ast'

            passed <- runLadderTest22 True test ast
            print (here, passed, if passed then "PASSED" else "FAILED")
        (_test, ["-n", value]) | Just n <- readMaybe value -> do
--             error $ show (here, "TODO")
            runLadderTest221 True n ast
        (Nothing, []) -> do
            print (here, "no embedded test found")
            runLadderTest221 True 10 ast
        other -> do
            error $ show (here, "TODO")

--     where
--     mp (a :< n) = a :< fmap mp (mapDg id id id n)
