
import System.FilePath.Glob (glob)
import Test.DocTest

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
    sourceFiles <- glob "src/Language/Ladder/*.hs"
    doctest
        $ "-XLambdaCase"
        : "-XScopedTypeVariables"
        : "-XTypeApplications"
        : "-XFlexibleContexts"
        : "-XTupleSections"
        : "-XRecordWildCards"
        : "-XDeriveFunctor"
        : "-XDeriveFoldable"
        : "-XPatternSynonyms"
        : "-XDeriveTraversable"
        : "-XCPP"
        : "-package ladder-logic"
        : sourceFiles
--         : ["src/Language/Ladder/Example.hs"]
--         : [
--               "src/Language/Ladder/Analysis.hs"
--             , "src/Language/Ladder/DiagramParser.hs"
--             , "src/Language/Ladder/Example.hs"
--             , "src/Language/Ladder/Interpreter.hs"
--             , "src/Language/Ladder/LadderParser.hs"
--             , "src/Language/Ladder/Lexer.hs"
--             , "src/Language/Ladder/OldBackend.hs"
--             , "src/Language/Ladder/Simple.hs"
--             , "src/Language/Ladder/Target.hs"
--             , "src/Language/Ladder/Utils.hs"
--             , "src/Language/Ladder/Zipper.hs"
--             ]
