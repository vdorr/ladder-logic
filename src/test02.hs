{-# OPTIONS_GHC -Wunused-imports #-}

{-# LANGUAGE CPP #-}

-- OverloadedStrings, 

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
-- import Data.Foldable
-- import Data.Traversable
import Data.List
import Text.Read
-- import Data.Maybe
-- import Data.Function
-- import Data.Bifunctor
import System.Environment (getArgs)
-- import Data.Tuple
-- import Control.Monad (replicateM_)
-- import Data.Semigroup

-- import Control.Monad.Writer.Strict

-- import Debug.Trace

import Preprocess

import Ladder.Zipper
import Ladder.Lexer -- (preproc5', runLexer, dropWhitespace)
import Ladder.DiagramParser
import Ladder.LadderParser

-- import NeatInterpolation

import Tooling

--------------------------------------------------------------------------------

data LadderTest = T01
    { testVect :: [(Int, [(String, V)])]
    , watch :: [String]
    , expected :: [[V]]
    } deriving (Show, Read)


runTest :: LadderTest -> Cofree (Diagram Dev String) DgExt -> IO ()
runTest test ast = do
    print here

    prog <- generateStk ast

    let xxy = evalTestVect'' prog (watch test) (testVect test)
    print (here, xxy)
    let Right tr2 = xxy
    putStrLn $ unlines $ prettyTrace $ zip (watch test) $ transpose tr2

    print (here, expected test == tr2)

    return ()

--------------------------------------------------------------------------------

main :: IO ()
main = do
    [file] <- getArgs
    src <- TIO.readFile file
    case stripPos <$> runLexer src of
        Left err -> TIO.putStrLn err
        Right x -> do
--             print (here, getPragma $ tokens x)
--             let Just pgma = fmap (filter (/='\\') . T.unpack) $getPragma $ tokens x
            let pgma = fmap (filter (/='\\') . T.unpack) $ getPragma $ tokens x
--             print ((read pgma) :: LadderTest)
            case pgma >>= readMaybe of
                Just test@T01{} -> do
                    print (here, test)

                    let zp = mkDgZp $ dropWhitespace x
#if 0
                    forM_ (zpToList zp) (print . (here,))
#endif

        --             print (here, "--------------------------------------------------")

                    case applyDgp test002' zp of
                        Right (ast, (DgPSt _ c@(Zp zpl zpr) _ _)) -> do
                            print (here, "--------------------------------------------------")
                            TIO.putStrLn src
        --                     testAst ast
                            runTest test ast
                        Left err -> print (here, err)
                Nothing -> do
                    print (here, "no embedded test found")
