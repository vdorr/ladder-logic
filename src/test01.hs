{-# OPTIONS_GHC -Wunused-imports #-}

{-# LANGUAGE CPP, TupleSections, TypeSynonymInstances, FlexibleInstances,
    QuasiQuotes, PatternSynonyms,TypeApplications,DeriveAnyClass,
    LambdaCase, ScopedTypeVariables, ViewPatterns, BangPatterns
    , FlexibleContexts #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import Data.Foldable
import System.Environment (getArgs)

import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Interpreter

-- import Tooling
-- import TestUtils

--------------------------------------------------------------------------------

testAst :: Cofree (Diagram () Dev String) DgExt -> IO ()
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
            let blocks = basicBlocks' lxs'

            forM_ blocks $ \(lbl, lxs'') -> do
                print (here, lbl)
                let zp = mkDgZp lxs''
                forM_ (zpToList zp) (print . (here,))
                case applyDgp parseLadderLiberal zp of
                    Left err -> print (here, err)
                    Right (ast, (DgPSt _ c@(Zp zpl zpr) _ _)) -> do
                        print (here, "--------------------------------------------------")
                        for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
--                     for_ zpr $ \q -> print (here, q)

                        print (here, "--------------------------------------------------")
                        TIO.putStrLn src
                        print (here, "--------------------------------------------------")

