{-# LANGUAGE OverloadedStrings #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Simple where

import Data.Traversable
import Data.Text (Text, unpack)
import Data.Void
import Data.Char (toUpper)

-- import Language.Ladder.Zipper
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
--     where
--     cmp = [">", "<", "=", "==", "<>", "/=", "!=", "≠", "≤", "≥"]
--     has2Ops (Contact_ f) = Right $ if elem f cmp then Mandatory else None
--     has2Ops _ = Right None
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
--     where
--     x = lookup (fmap unpack d) devices

--------------------------------------------------------------------------------

--FIXME IO
generateStk3
    :: (Show address, Show word, Show device)
    => (Int -> IO word) --XXX fix that IO thing already !!! OMG
    -> (device -> [Instruction word address])
    -> [(Maybe String, Cofree (Diagram Void device String) DgExt)]
    -> IO [ExtendedInstruction Int word address]
generateStk3 literalFromInt doDevice ast = do
    Right ast'
        <- resolveLabels <$> for ast (traverse (generateStk2' literalFromInt doDevice))
--     Right ast'' <- return $ resolveLabels ast' -- AAAAAAAAAAAAAAAAAAAA
    return ast'

--------------------------------------------------------------------------------

-- XXX i increasingly feel like Map String [Instruction]
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
    -- | FB String [(String, D)] [(String, D)]
    deriving (Show) -- , Functor)

data CmpOp = Lt | Gt | Lte | Gte | Eq | NEq
    deriving Show

--------------------------------------------------------------------------------

--FIXME IO
generateStk2
    :: (Show lbl, Eq lbl, Show addr)
    => (dev -> Either String x)
    -> (x -> [Instruction Int addr])
    -> Cofree (Diagram Void dev lbl) DgExt
    -> IO [ExtendedInstruction lbl Int addr]
-- generateStk2 = generateStk2' pure emitBasicDevice . parseOps
generateStk2 doOp emitDev ast = do
    Right ast' <- return $ mapOpsM doOp ast
    generateStk2' pure emitDev ast'

--FIXME IO
generateStk2xx
    :: (Show address, Show word, Show lbl, Eq lbl)
    => (Int -> IO word) --XXX fix that IO thing already !!! OMG
    -> [(Maybe lbl, Cofree (Diagram Void (Op String (Operand address)) lbl) DgExt)]
    -> IO [ExtendedInstruction Int word address]
generateStk2xx literalFromInt ast = do
    ast' <- for ast (traverse (generateStk2' literalFromInt emitBasicDevice))
    Right ast'' <- return $ resolveLabels ast' -- AAAAAAAAAAAAAAAAAAAA
    return ast''

--------------------------------------------------------------------------------

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

--FIXME do this in LadderParser
-- parseOps
--     :: Cofree (Diagram c (Dev String)            s) p
--     -> Cofree (Diagram c (Op s (Operand String)) s) p
-- parseOps = either (error here) id . parseOpsM

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
