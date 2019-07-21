{-# OPTIONS_GHC -Wunused-imports  -Wall #-}
{-# LANGUAGE CPP, TupleSections #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Foldable
-- import Data.Void

import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Simple

--------------------------------------------------------------------------------

-- testAst :: Cofree (Diagram Void (Dev String) String) DgExt -> IO ()
-- testAst ast' = do
--     generateStk2 ast'
--     return ()

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
                for_ (zpToList zp) (print . (here,))
                case runLadderParser wrapDevice3 ladderLiberal lxs'' of
                    Left err -> print (here, err)
                    Right (_ast, zp1) -> do
                        print (here, "--------------------------------------------------")
                        for_ (zpToList zp1) (print . (here,))
                        print (here, "--------------------------------------------------")
                        TIO.putStrLn src
                        print (here, "--------------------------------------------------")
