{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE CPP, TupleSections, FlexibleInstances, QuasiQuotes, LambdaCase,
    ScopedTypeVariables, FlexibleContexts #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Foldable
import Data.Void

import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Interpreter
import Language.Ladder.Simple

-- import Tooling
-- import TestUtils

--------------------------------------------------------------------------------

testAst :: Cofree (Diagram Void (Dev String) String) DgExt -> IO ()
testAst ast' = do
    generateStk2 ast'
    return ()

--------------------------------------------------------------------------------

main :: IO ()
main = do
    [file] <- getArgs
    src <- TIO.readFile file
    case stripPos <$> runLexer src of
        Left err -> TIO.putStrLn err
        Right lxs -> do

            let lxs' = dropWhitespace lxs
            let blocks = labeledRungs lxs'

            forM_ blocks $ \(lbl, lxs'') -> do
                print (here, lbl)
                let zp = mkDgZp lxs''
                forM_ (zpToList zp) (print . (here,))
                case runLadderParser parseLadderLiberal lxs'' of
                    Left err -> print (here, err)
                    Right (ast, c@(Zp zpl zpr)) -> do
                        print (here, "--------------------------------------------------")
                        for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
--                     for_ zpr $ \q -> print (here, q)

                        print (here, "--------------------------------------------------")
                        TIO.putStrLn src
                        print (here, "--------------------------------------------------")

