{-# LANGUAGE OverloadedStrings #-}

module Language.Ladder.Simple where

import Data.Text (Text)

-- import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser

--------------------------------------------------------------------------------

data Dev t = Dev !(DevType t) ![Operand t]
    deriving (Show, Eq, Functor)

runLadderParser_
    :: LdP (Dev Text) Text a
    -> [(Int, [((Int, Int), Tok Text)])]
    -> Either String a
runLadderParser_ p s = fst <$> runLadderParser p s

runLadderParser
    :: LdP (Dev Text) Text a
    -> [(Int, [((Int, Int), Tok Text)])]
    -> Either String (a, Dg (Tok Text))
runLadderParser = runParser wrapDevice
--     where
--     cmp = [">", "<", "=", "==", "<>", "/=", "!=", "≠", "≤", "≥"]
--     has2Ops (Contact_ f) = Right $ if elem f cmp then Mandatory else None
--     has2Ops _ = Right None
-- 
--     mkDev d = (, pure . Dev d) <$> has2Ops d

wrapDevice
    :: DevType Text
    -> Either String
        ( DevOpFlag
        , [Operand Text] -> Either String (Dev Text)
        )
wrapDevice d = (, pure . Dev d) <$> has2Ops d
    where
    cmp = [">", "<", "=", "==", "<>", "/=", "!=", "≠", "≤", "≥"]
    has2Ops (Contact_ f) = Right $ if elem f cmp then Mandatory else None
    has2Ops _ = Right None

