{-# OPTIONS_GHC -Wunused-imports -Wall #-}
{-# LANGUAGE CPP #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Bifunctor

import Language.Ladder.Interpreter --FIXME
import Language.Ladder.Simple --FIXME

import TestUtils
import Language.Ladder.LadderParser
import Language.Ladder.Utils

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
            ast <- either fail pure $ parseOrDie2 (wrapDevice3 (pure . I) (pure . A)) lxs
--             let ast = fmap (second (mapDg id snd id)) ast'
--             let ast = fmap (second (mp)) ast'

            passed <- runLadderTest2 True test ast
            print (here, passed, if passed then "PASSED" else "FAILED")
        Nothing -> print (here, "no embedded test found")

--     where
--     mp (a :< n) = a :< fmap mp (mapDg id id id n)
