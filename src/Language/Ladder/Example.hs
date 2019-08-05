{-# LANGUAGE OverloadedStrings #-}

module Language.Ladder.Example where

import qualified Data.Text

import Language.Ladder.Simple
import Language.Ladder.LadderParser
import Language.Ladder.Lexer
import Language.Ladder.Utils
import Language.Ladder.DiagramParser

import Data.Void

--------------------------------------------------------------------------------

parseLd :: Data.Text.Text
                      -> Either
                           String
                           (Language.Ladder.Utils.Cofree
                              (Diagram
                                 Data.Void.Void
                                 (DevType Data.Text.Text, [Operand Data.Text.Text])
                                 Data.Text.Text)
                              Language.Ladder.DiagramParser.DgExt)
parseLd s = do
    ast <- (stripPos . dropWhitespace) <$> runLexer' s
    runLadderParser_ wrapDeviceSimple ladder ast

--------------------------------------------------------------------------------

test :: Data.Text.Text
test
    = Data.Text.unlines
    [ "|       Start     Stop   Run "
    , "+--+----[ ]--+----[/]----( )-"
    , "   |         |               "
    , "   |    Run  |               "
    , "   +----[ ]--+               "
    ]

-- | test
--
-- Examples:
--
-- >>> testtest 1
-- 2
testtest :: Int -> Int
testtest = (+1)
