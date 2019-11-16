{-# LANGUAGE OverloadedStrings, QuantifiedConstraints #-}

module Language.Ladder.Example
    (LadderAst, PPCofree(..), parseLd)
    where

import Data.Text (Text)
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
-- >>> :{
--  fmap PPCofree $ parseLd $ Data.Text.unlines
--      [ "|  A    B   "
--      , "+--[ ]--( )-" ]
-- :}
-- Right (
-- (1,(1,1))    :< Source (
-- (2,(1,1))    :< Node [(
-- (2,(4,6))    :< Device (Contact_ " ",[Var "A"]) (
-- (2,(9,11))   :< Device (Coil_ " ",[Var "B"]) (
-- (2,(13,13))  :< Sink)))]))
--
-- >>> :{
--  fmap PPCofree $ parseLd $ Data.Text.unlines
--      [ "|       Start     Stop   Run "
--      , "+--+----[ ]--+----[/]----( )-"
--      , "   |         |               "
--      , "   |    Run  |               "
--      , "   +----[ ]--+               " ]
-- :}
-- Right (
-- (1,(1,1))    :< Source (
-- (2,(1,1))    :< Node [(
-- (2,(4,4))    :< Node [(
-- (2,(9,11))   :< Device (Contact_ " ",[Var "Start"]) (
-- (2,(14,14))  :< Node [(
-- (2,(19,21))  :< Device (Contact_ "/",[Var "Stop"]) (
-- (2,(26,28))  :< Device (Coil_ " ",[Var "Run"]) (
-- (2,(30,30))  :< Sink))),(
-- (5,(14,14))  :< Node [])])),(
-- (5,(4,4))    :< Node [(
-- (5,(9,11))   :< Device (Contact_ " ",[Var "Run"]) (
-- (5,(14,14))  :< Sink))])])]))
parseLd :: Text -> Either String LadderAst
parseLd s = do
    ast <- (stripPos3 . dropWhitespace2) <$> runLexer' s
    runLadderParser_ wrapDeviceSimple ladder ast

--------------------------------------------------------------------------------

-- | 'Cofree' wrapper with alternative 'Show' instance
newtype PPCofree f a = PPCofree (Cofree f a)

instance (Functor f, forall t. Show t => Show (f t), Show a) => Show (PPCofree f a) where
    show (PPCofree ast) = pp ast
        where
--         pp :: (Functor f, Show a, forall t. Show t => Show (f t)) => Cofree f a -> String
        pp (a :< n) = "(\n" ++ pad (show a) ++ " :< " ++ show (fmap (Nope . pp) n)  ++ ")"
        pad = take 12 . (++repeat ' ')

newtype Nope = Nope String
instance Show Nope where
    show (Nope s) = s

--------------------------------------------------------------------------------
