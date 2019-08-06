{-# LANGUAGE OverloadedStrings #-}

module Language.Ladder.Example where

import qualified Data.Text
import Data.Text (Text, unlines)
import Data.Void

import Language.Ladder.Simple
import Language.Ladder.LadderParser
import Language.Ladder.Lexer
import Language.Ladder.Utils
import Language.Ladder.DiagramParser

--------------------------------------------------------------------------------

type LadderAst = Cofree (Diagram Void (DevType Text, [Operand Text]) Text) DgExt

-- | parseLd
--
-- Examples:
--
-- >>> parseLd test1
-- Right (
-- (1,(1,1))    :< Source (
-- (2,(1,1))    :< Node [(
-- (2,(4,6))    :< Device (Contact_ " ",[Var "A"]) (
-- (2,(9,11))   :< Device (Coil_ " ",[Var "B"]) (
-- (2,(13,13))  :< Sink)))]))
parseLd :: Text -> Either String LadderAst
parseLd s = do
    ast <- (stripPos . dropWhitespace) <$> runLexer' s
    runLadderParser_ wrapDeviceSimple ladder ast

--------------------------------------------------------------------------------

test1 :: Text
test1
    = Data.Text.unlines
    [ "|  A    B   "
    , "+--[ ]--( )-"
    ]

test2 :: Text
test2
    = Data.Text.unlines
    [ "|       Start     Stop   Run "
    , "+--+----[ ]--+----[/]----( )-"
    , "   |         |               "
    , "   |    Run  |               "
    , "   +----[ ]--+               "
    ]

-- data Nope = Nope String
-- instance Show Nope where
--     show (Nope s) = s
-- 
-- pp :: (Functor f, Show a, forall t. Show t => Show (f t)) => Cofree f a -> String
-- pp (a :< n) = "(\n" ++ pad (show a) ++ " :< " ++ show (fmap (Nope . pp) n)  ++ ")"
--     where
--     pad = take 12 . (++repeat ' ')
