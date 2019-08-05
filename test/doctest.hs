-- module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest -- (doctest)
-- import Build_doctests (flags, pkgs, module_sources)
-- import Data.Foldable (traverse_)
-- import Test.DocTest (doctest)

import Language.Ladder.Simple
import Language.Ladder.LadderParser
import Language.Ladder.Lexer
import Language.Ladder.Utils
import Language.Ladder.DiagramParser

main :: IO ()
-- main = glob "src/**/*.hs" >>= doctest
-- main = doctest ["-isrc", "src/Language/Ladder/Example.hs"]
-- main = glob "src/Language/Ladder/Example.hs" >>= doctest 
-- main = do
--     traverse_ putStrLn args
--     doctest args
--   where
--     args = flags ++ pkgs ++ module_sources
main = do
    sourceFiles <- glob "src/Language/Ladder/Example.hs"
    doctest
        $ "-XConstraintKinds"
        : "-XLambdaCase"
        : "-XScopedTypeVariables"
        : "-XTypeApplications"
        : "-XViewPatterns"
        : "-XTupleSections"
        : "-XGeneralizedNewtypeDeriving"
        : "-XDeriveGeneric"
        : "-XDeriveFunctor"
        : "-XDeriveFoldable"
        : "-XPatternSynonyms"
        : "-XDeriveTraversable"
        : "-XCPP"
--         : sourceFiles
        : [
              "src/Language/Ladder/Analysis.hs"
            , "src/Language/Ladder/DiagramParser.hs"
            , "src/Language/Ladder/Example.hs"
            , "src/Language/Ladder/Interpreter.hs"
            , "src/Language/Ladder/LadderParser.hs"
            , "src/Language/Ladder/Lexer.hs"
            , "src/Language/Ladder/OldBackend.hs"
            , "src/Language/Ladder/Simple.hs"
            , "src/Language/Ladder/Target.hs"
            , "src/Language/Ladder/Utils.hs"
            , "src/Language/Ladder/Zipper.hs"
            ]
