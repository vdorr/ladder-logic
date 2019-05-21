{-# LANGUAGE CPP, TupleSections, TypeSynonymInstances, FlexibleInstances,
    QuasiQuotes, PatternSynonyms,TypeApplications,
    LambdaCase, ScopedTypeVariables, ViewPatterns, BangPatterns, FlexibleContexts #-}

-- OverloadedStrings, 

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Foldable
import Data.Traversable
import Data.List
import Text.Read
import Data.Maybe
import Data.Function
import Data.Bifunctor
import System.Environment (getArgs)
import Data.Tuple
import Control.Monad (replicateM_)
import Data.Semigroup

import Control.Monad.Writer.Strict

import Debug.Trace

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

--------------------------------------------------------------------------------

main :: IO ()
main = do
--     print ((readEither tst01):: Either String LadderTest)
--     print $ T01 [] [] []
--     print ((read tst02):: LadderTest)

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
                 Nothing -> do
                     print (here, "no embedded test found")

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
                Left err -> print (here, err)
