{-# OPTIONS_GHC -Wunused-imports -Wall #-}

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Read

import Language.Ladder.Interpreter --FIXME
import Language.Ladder.Simple --FIXME

import TestUtils

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
            print ("embedded test found:", test)
            passed <- runLadderTest22 True test ast
            print ("test result:", passed, if passed then "PASSED" else "FAILED")
        (_test, ["-n", value]) | Just n <- readMaybe value -> do
            runLadderTest221 True n ast
        (Nothing, []) -> do
            print ("no embedded test found")
            runLadderTest221 True 10 ast
        other -> error $ show ("TODO", other)
