
import System.FilePath.Glob (glob)
import Test.DocTest

main :: IO ()
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
        : "-XOverloadedStrings"
--         : "-package ladder-logic"
        : sourceFiles
