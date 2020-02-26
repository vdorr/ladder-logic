
module Language.Ladder.Simple where

import Data.Void
import Data.String

import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Types

--------------------------------------------------------------------------------

runLadderParser_
    :: DeviceParser t d -- ^device recognizer
    -> LdP d t a -- ^parser
    -> [[(DgExt, Tok t)]] -- ^input tokens
    -> Either String a -- ^parser result
runLadderParser_ pd p s = fst <$> runParser pd p s

--------------------------------------------------------------------------------

type Ast4 t = [(Maybe t, Cofree (Diagram Void (DevType t, [Operand t]) t) DgExt)]

parseLadder4
    :: [[(DgExt, Tok t)]]
    -> Either String (Ast4 t)
parseLadder4 lxs = do
    let lxs' = dropWhitespace lxs
    fst <$> runParser wrapDeviceSimple ladder' lxs'

-- parseLadder1
--     :: String
--     -> Either String Ast4
-- parseLadder1 s = do
--     ast <- dropWhitespace <$> runLexerS' s
--     runLadderParser_ wrapDeviceSimple ladder' ast

--------------------------------------------------------------------------------

-- |Accept any device.
-- Does not allow upper operand of neighbor device to overlap current device
wrapDeviceSimple :: DeviceParser name (DevType name, [Operand name])
wrapDeviceSimple dt = Right (Optional, Right . (dt,))

-- |Accept comparison operators.
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
