#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module TestUtils where

import Data.List
import Text.Read
import qualified Data.Text.IO as TIO
import Data.Text (Text, unpack)
-- import qualified Data.Text as T
import Control.Monad

import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Interpreter

import Tooling

--------------------------------------------------------------------------------

data LadderTest = T01
    { testVect :: [(Int, [(String, V)])]
    , watch :: [String]
    , expected :: [[V]]
    } deriving (Show, Read)

--------------------------------------------------------------------------------

getSignal :: String -> TestVect -> [[V]] -> [[V]]
getSignal s ((_, slice) : _) = fmap ((:[]).(!!i))
    where
    Just i = findIndex ((s==) . fst) slice

getSignals :: [String] -> TestVect -> [[V]] -> [[V]]
getSignals sg vect trace = 
    foldMap (\s -> getSignal s vect trace) sg

runLadderTest :: Bool -> LadderTest -> Cofree (Diagram () Dev String) DgExt -> IO Bool
runLadderTest verbose test@T01{} ast = do
    when verbose $ print here

    prog <- generateStk ast
--     prog <- generateStk2 ast

    let allSigs = testVectSignals (testVect test)
    let displaySigs = allSigs -- or (watch test)
    when verbose $ print (here, allSigs)

    let xxy = evalTestVect'' prog allSigs (testVect test)
--     let xxy = evalTestVect'' prog (watch test) (testVect test)
    when verbose $ print (here, xxy)
    let Right traces = xxy
--     when verbose $ putStrLn $ unlines $ prettyTrace $ zip (watch test) $ transpose traces
    when verbose $ putStrLn $ unlines $ prettyTrace $ zip allSigs $ transpose traces

--     let idxs = fmap (findIndex)
    let testTrace = getSignals (watch test) (testVect test) traces
    when verbose $ print (here, testTrace)
    when verbose $ print (here, expected test)

    let passed = expected test == testTrace
--     let passed = False
    when verbose $ print (here, passed, if passed then "PASSED" else "FAILED")

    return passed

--------------------------------------------------------------------------------

parseOrDie
    :: [(Int, [((Int, Int), Tok Text)])]
    -> IO (Cofree (Diagram () Dev String) DgExt)
parseOrDie lxs = do
    let zp = mkDgZp $ dropWhitespace lxs
#if 0
    forM_ (zpToList zp) (print . (here,))
#endif
    case applyDgp parseLadder zp of
        Right (ast, (DgPSt _ c@(Zp zpl zpr) _ _)) -> do
--             print (here, "--------------------------------------------------")
            return ast
        Left err -> fail $ show (here, err)


loadLadderTest :: FilePath -> IO (Maybe LadderTest, [(Int, [((Int, Int), Tok Text)])])
loadLadderTest file = do
    src <- TIO.readFile file
    case stripPos <$> runLexer src of
        Left err -> fail $ show (here, err)
        Right x -> do
--             print (here, getPragma $ tokens x)
--             let Just pgma = fmap (filter (/='\\') . T.unpack) $getPragma $ tokens x
            let pgma = fmap (filter (/='\\') . unpack) $ getPragma $ dropPos x
--             print ((read pgma) :: LadderTest)
            return (pgma >>= readMaybe, x)
--                     fail $ show (here, "no embedded test found")
