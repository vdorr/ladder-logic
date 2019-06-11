{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE CPP #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
-- import qualified Data.Text as T
-- import Data.List
-- import Text.Read
import System.Environment (getArgs)

-- import Preprocess
-- 
-- import Ladder.Zipper
-- import Ladder.Lexer
-- import Ladder.DiagramParser
-- import Ladder.LadderParser
-- import Language.Ladder.Utils

-- import NeatInterpolation

-- import Tooling
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
            ast <- parseOrDie lxs
            runLadderTest test ast
        Nothing -> print (here, "no embedded test found")

-- main :: IO ()
-- main = do
--     [file] <- getArgs
--     src <- TIO.readFile file
--     case stripPos <$> runLexer src of
--         Left err -> TIO.putStrLn err
--         Right x -> do
-- --             print (here, getPragma $ tokens x)
-- --             let Just pgma = fmap (filter (/='\\') . T.unpack) $getPragma $ tokens x
--             let pgma = fmap (filter (/='\\') . T.unpack) $ getPragma $ tokens x
-- --             print ((read pgma) :: LadderTest)
--             case pgma >>= readMaybe of
--                 Just test@T01{} -> do
--                     print (here, test)
-- 
--                     let zp = mkDgZp $ dropWhitespace x
-- #if 0
--                     forM_ (zpToList zp) (print . (here,))
-- #endif
-- 
--         --             print (here, "--------------------------------------------------")
-- 
--                     case applyDgp test002' zp of
--                         Right (ast, (DgPSt _ c@(Zp zpl zpr) _ _)) -> do
--                             print (here, "--------------------------------------------------")
--                             TIO.putStrLn src
--         --                     testAst ast
--                             runLadderTest test ast
--                         Left err -> print (here, err)
--                 Nothing -> do
--                     print (here, "no embedded test found")
