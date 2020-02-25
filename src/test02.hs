{-# OPTIONS_GHC -Wunused-imports -Wall #-}

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Text.Read

import Language.Ladder.Interpreter --FIXME
import Language.Ladder.Simple --FIXME

import TestUtils

--------------------------------------------------------------------------------
import Control.Monad
import Language.Ladder.Eval

evalLadder4 verbose num ast = do
    when verbose $ print "1"
    let q = evalTestVect1 ast
--     prog <- either fail pure $ compileForTest03 ast
--     let memSlots' :: [(CellType, Text)] = nub $ execWriter $ traverse_ (traverse_ mp2) ast
--     let memSlots = fmap (fmap unpack) memSlots'
--     print (here, memSlots, "<<<<<"::String)
--     let vect = memSlotsToTestVector num memSlots
--     print (here, vect, "<<<<<"::String)
-- 
--     when verbose $ do
--         putStrLn "---------------------------"
--         for_ prog print
--         putStrLn "---------------------------"
--     let allSigs = fmap snd memSlots
--     let xxy = evalTestVect''' prog allSigs vect
-- 
-- --     when verbose $ print (here, xxy)
--     let Right traces = xxy
-- 
--     print (here, traces)
--     when verbose $ putStrLn $ unlines $ prettyTrace $ zip allSigs $ transpose traces
    return ()

main :: IO ()
main = do
    file : otherArgs <- getArgs
    src <- TIO.readFile file
    (tst, lxs) <- loadLadderTest file
    TIO.putStrLn src
    ast <- either fail pure $ parseOrDie2 (wrapDevice3 (pure . I) (pure . A)) lxs

    ast' <- either fail pure $ parseOrDie2 (wrapDeviceSimple) lxs
    evalLadder4 True 10 ast'

    case (tst, otherArgs) of
        (Just test, []) -> do
            print ("embedded test found:", test)
            passed <- runLadderTest22 True test ast
            print ("test result:", passed, if passed then "PASSED" else "FAILED")
        (_test, ["-n", value]) | Just n <- readMaybe value -> do
            evalLadder221 True n ast
        (Nothing, []) -> do
            print ("no embedded test found")
            evalLadder221 True 10 ast
        other -> error $ show ("TODO", other)
