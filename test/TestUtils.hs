#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module TestUtils where

import Data.List
import Text.Read
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Ladder.Zipper
import Ladder.Lexer
import Ladder.DiagramParser
import Ladder.LadderParser
import Language.Ladder.Utils

import Tooling

--------------------------------------------------------------------------------

data LadderTest = T01
    { testVect :: [(Int, [(String, V)])]
    , watch :: [String]
    , expected :: [[V]]
    } deriving (Show, Read)

--------------------------------------------------------------------------------

runLadderTest :: LadderTest -> Cofree (Diagram () Dev String) DgExt -> IO ()
runLadderTest test@T01{} ast = do
    print here

    prog <- generateStk ast

    let xxy = evalTestVect'' prog (watch test) (testVect test)
    print (here, xxy)
    let Right tr2 = xxy
    putStrLn $ unlines $ prettyTrace $ zip (watch test) $ transpose tr2

    let passed = expected test == tr2
    print (here, passed, if passed then "PASSED" else "FAILED")

    return ()

--------------------------------------------------------------------------------

parseOrDie
    :: [(Int, [((Int, Int), Tok T.Text)])]
    -> IO (Cofree (Diagram () Dev String) DgExt)
parseOrDie lxs = do
    let zp = mkDgZp $ dropWhitespace lxs
#if 0
    forM_ (zpToList zp) (print . (here,))
#endif
    case applyDgp test002' zp of
        Right (ast, (DgPSt _ c@(Zp zpl zpr) _ _)) -> do
            print (here, "--------------------------------------------------")
            return ast
        Left err -> fail $ show (here, err)


loadLadderTest :: FilePath -> IO (Maybe LadderTest, [(Int, [((Int, Int), Tok T.Text)])])
loadLadderTest file = do
    src <- TIO.readFile file
    case stripPos <$> runLexer src of
        Left err -> fail $ show (here, err)
        Right x -> do
--             print (here, getPragma $ tokens x)
--             let Just pgma = fmap (filter (/='\\') . T.unpack) $getPragma $ tokens x
            let pgma = fmap (filter (/='\\') . T.unpack) $ getPragma $ tokens x
--             print ((read pgma) :: LadderTest)
            return (pgma >>= readMaybe, x)
--                     fail $ show (here, "no embedded test found")