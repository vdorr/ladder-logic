{-# LANGUAGE OverloadedStrings #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Simple where

import Data.Traversable
import Data.Text (Text, unpack)
import Data.Void
import Data.Char (toUpper)

import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Interpreter
import Language.Ladder.Utils

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

--------------------------------------------------------------------------------

-- wrapDevice2
--     :: DevType Text
--     -> Either String
--         ( DevOpFlag
--         , [Operand Text] -> Either String (Dev Text)
--         )
wrapDevice2 d
    = case lookup (fmap unpack d) devices of
        Just dd@(DDesc _name ty _impl)
            -> Right (if length ty > 1 then Mandatory else None
                , \ops -> Right (ops, dd))
        Nothing -> Left "device type unknown"

--------------------------------------------------------------------------------

literalFromInt :: (Bounded a, Integral a) => Int -> IO a
literalFromInt i = return $ fromIntegral i --TODO check range

generateStk2xx
    :: (Show addr, Show word, Show lbl, Eq lbl)
    => (dev -> Either String x)
    -> (x -> [Instruction word addr])
    -> (Int -> IO word) --XXX fix that IO thing already !!! OMG
    -> [(Maybe lbl, Cofree (Diagram Void dev lbl) DgExt)]
    -> IO [ExtendedInstruction Int word addr]
generateStk2xx doOp emitDev literalFromInt ast = do
    Right ast'   <- return $ for ast (traverse (mapOpsM doOp))
    ast''        <- for ast' (traverse (generateStk2' literalFromInt emitDev))
    Right ast''' <- return $ resolveLabels ast'' -- AAAAAAAAAAAAAAAAAAAA
    return ast'''

--------------------------------------------------------------------------------

data Op s n
    = And       n -- wire out <- wire in and memory cell
    | AndN      n
    | Ld          -- ^ set wire state same as argument
    | On          -- ^ set wire state to #on
    | St        n
    | StN       n -- | StOn | StOff
    | LdP       n -- rising edge detect
    | LdN       n -- falling edge detect
    | Jmp s
    | Cmp CmpOp n n
    deriving (Show) -- , Functor)

data CmpOp = Lt | Gt | Lte | Gte | Eq | NEq
    deriving Show

--------------------------------------------------------------------------------

mapOperandA
    :: (Applicative m)
    => (CellType -> t -> m n)
    -> Op s t
    -> m (Op s n)
mapOperandA doOperand = f
    where
    f (And    a  ) =        And    <$> doOperand Bit a
    f (AndN   a  ) =        AndN   <$> doOperand Bit a
    f (St     a  ) =        St     <$> doOperand Bit a
    f (StN    a  ) =        StN    <$> doOperand Bit a
    f (Cmp op a b) =        Cmp op <$> doOperand Word a <*> doOperand Word b
    f  Ld          = pure   Ld
    f (LdP    a  ) =        LdP    <$> doOperand TwoBits a
    f (LdN    a  ) =        LdN    <$> doOperand TwoBits a
    f  On          = pure   On
    f (Jmp s)      = pure $ Jmp s

emitBasicDevice
    :: (Show address) -- , Show op)
    => Op op (Operand address)
    -> [Instruction word address]
emitBasicDevice d
    = case d of
        And  (Var addr)  -> [ILdBit addr, IAnd]
        AndN (Var addr)  -> [ILdBit addr, INot, IAnd]
        St   (Var addr)  -> [IStBit addr]
        StN  (Var addr)  -> [INot, IStBit addr, INot]
        _                -> error here -- $ show (here, d)

mapOpsM
    :: Monad m => (a -> m b)
    -> Cofree (Diagram c a s) p
    -> m (Cofree (Diagram c b s) p)
mapOpsM f (a :< n) = (a :<) <$> (mapDgA pure f pure n >>= traverse (mapOpsM f))

parseOpsM
    :: Cofree (Diagram c (Dev String) s) p
    -> Either String (Cofree (Diagram c (Op s (Operand String)) s) p)
parseOpsM = mapOpsM parseOp

parseOp
    :: Dev String
    -> Either String (Op s (Operand String))
parseOp = f
    where
    f (Dev (Coil_ op) arg) = case (fmap toUpper op, arg) of
        (" ", [n]   ) -> pure $ St n
        ("/", [n]   ) -> pure $ StN n
        ("R", [n]   ) -> undefined
        ("S", [n]   ) -> undefined
        _               -> Left "unknown coil type"
    f (Dev (Contact_ op) arg) = case (fmap toUpper op, arg) of
        (" ", [n]   ) -> pure $ And  n
        ("/", [n]   ) -> pure $ AndN  n
        (">", [a, b]) -> pure $ Cmp Gt a b
        ("P", [n]   ) -> pure $ LdP n
        ("N", [n]   ) -> pure $ LdN n
        _               -> Left "unknown contact type"

--------------------------------------------------------------------------------
