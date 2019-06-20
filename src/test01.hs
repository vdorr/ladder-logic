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

    let watch = ["b", "d"]
    let memory =
                [ ("a", X True),("b", X False),("c", X False),("d", X False)
--                 , ("%QX0", X True), ("%IX0", I 0)
                ]
#if 0
    xxx <- generateStk ast'
    for_ xxx print
    let watch2 = ["a","b","c","d"]
    let xxy = evalTestVect'' xxx watch2 vect01
    print (here, xxy)
    let Right tr2 = xxy
    putStrLn $ unlines $ prettyTrace $ zip watch2 $ transpose tr2
#endif

    generateStk2 ast'
    return ()

#if 0

    let ast = parseOps ast'
    let (st, op, cnt) = fffff ast
    print (here, "-----------------------")
    let Just p01 = tsort [] $ or'd [] op
    print (here, "memory after single eval:", network' p01 memory)

    print (here, "-----------------------")
    for_ p01 print

    print (here, "-----------------------")
    print (here, "test trace:")

    let !trace = evalTestVect p01 watch vect01
    print (here, trace)
    putStrLn $ unlines $ prettyTrace $ zip watch $ transpose trace
#endif

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

