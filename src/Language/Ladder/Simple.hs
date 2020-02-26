
module Language.Ladder.Simple where

-- import Data.Traversable
-- import Data.Text (Text)
import Data.Void
import Data.String

-- import Control.Monad.Except

import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
-- import Language.Ladder.Interpreter
import Language.Ladder.Utils
import Language.Ladder.Types

import Language.Ladder.Eval

--------------------------------------------------------------------------------

-- |Execute diagram parser and return its result along with final stream state
runLadderParser
    :: DeviceParser t d -- ^device recognizer
    -> LdP d t a -- ^parser
    -> [[(DgExt, Tok t)]] -- ^input tokens
    -> Either String (a, Dg (Tok t)) -- ^parser result and final state of parser stream
runLadderParser = runParser

-- |Like 'runLadderParser' but discards parser stream
runLadderParser_
    :: DeviceParser t d -- ^device recognizer
    -> LdP d t a -- ^parser
    -> [[(DgExt, Tok t)]] -- ^input tokens
    -> Either String a -- ^parser result
runLadderParser_ pd p s = fst <$> runParser pd p s

--------------------------------------------------------------------------------

type Ast4 = [(Maybe String, Cofree (Diagram Void (DevType String, [Operand String]) String) DgExt)]

parseLadder1'
    :: [[(DgExt, Tok String)]]
    -> Either String Ast4
parseLadder1' lxs = do
    let lxs' = dropWhitespace lxs
    runLadderParser_ wrapDeviceSimple ladder' lxs'

parseLadder1
    :: String
    -> Either String Ast4
parseLadder1 s = do
    ast <- dropWhitespace <$> runLexerS' s
    runLadderParser_ wrapDeviceSimple ladder' ast

eval1 :: Ast4
    -> EvMem String
    -> EvMem String
eval1 = Language.Ladder.Eval.eval

--------------------------------------------------------------------------------

-- |Accept any device.
-- Does not allow upper operand of neighbor device to overlap current device
wrapDeviceSimple :: DeviceParser name (DevType name, [Operand name])
wrapDeviceSimple dt = Right (Optional, Right . (dt,))

-- |Accept comporason operators.
-- Upper operand of neighbor device may overlap current device.
wrapDeviceSimple2
    :: (Eq name, IsString name)
    => DeviceParser name (DevType name, [Operand name])
wrapDeviceSimple2 dt = Right (has2Ops dt, Right . (dt,))
    where
    cmp = fromString <$> [">", "<", "=", "==", "<>", "/=", "!=", "≠", "≤", "≥"]
    has2Ops (Contact_ f) = if elem f cmp then Mandatory else None
    has2Ops _            = None

--------------------------------------------------------------------------------
