{-# LANGUAGE OverloadedStrings #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Simple where

import Data.Traversable
import Data.Text (Text, unpack)
import Data.Void
import Data.Char (toUpper)

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Lazy as M
import Data.Word

import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Interpreter
import Language.Ladder.Utils

--------------------------------------------------------------------------------

runLadderParser_
    :: DeviceParser t d
    -> LdP d t a
    -> [(Int, [((Int, Int), Tok t)])]
    -> Either String a
runLadderParser_ pd p s = fst <$> runLadderParser pd p s

-- runLadderParser
--     :: LdP (Dev Text) Text a
--     -> [(Int, [((Int, Int), Tok Text)])]
--     -> Either String (a, Dg (Tok Text))
-- runLadderParser = runLadderParser' parseSimpleDevice

runLadderParser
    :: DeviceParser t d
    -> LdP d t a
    -> [(Int, [((Int, Int), Tok t)])]
    -> Either String (a, Dg (Tok t))
runLadderParser = runParser

-- parseSimpleDevice
--     :: DevType Text
--     -> Either String
--         ( DevOpFlag
--         , [Operand Text] -> Either String (Dev Text)
--         )
-- parseSimpleDevice d = (, pure . Dev d) <$> has2Ops d
--     where
--     cmp = [">", "<", "=", "==", "<>", "/=", "!=", "≠", "≤", "≥"]
--     has2Ops (Contact_ f) = Right $ if elem f cmp then Mandatory else None
--     has2Ops _ = Right None

--------------------------------------------------------------------------------

type DeviceParser name dev = DevType name
    -> Either String
        ( DevOpFlag
        , [Operand name] -> Either String dev
        )

-- devices1 :: Devices word addr Text
-- devices1 = devices pure


-- parseSimpleDevice :: DeviceParser Text (Dev Text)
-- parseSimpleDevice d
--     = case lookup d devices1 of
--         Just dd@(DDesc _name ty _impl)
--             -> Right (if length ty > 1 then Mandatory else None
--                     , \ops -> Right $ Dev d ops)
--         Nothing -> Left "device type unknown"

-- wrapDevice3
--     :: (Integral word, Integral addr)
--     => DevType Text
--     -> Either String
--         ( DevOpFlag
--         , [Operand Text] -> Either String
--             ([(CellType, Operand Text)], DeviceImpl word addr)
--         )
wrapDevice3
    :: (Int -> Either String word)
    -> (addr -> Either String word)
    -> DeviceParser Text ([(CellType, Operand Text)], DeviceImpl word addr)
wrapDevice3 mkWord litFromAddr d
    = case lookup d (devices mkWord litFromAddr) of
        Just dd@(DDesc _name ty impl)
            -> Right (if length ty > 1 then Mandatory else None
                    , \ops -> Right (zip (fmap snd ty) ops, impl))
        Nothing -> Left "device type unknown"

--------------------------------------------------------------------------------

literalFromInt :: (Bounded a, Integral a) => Int -> IO a
literalFromInt i = return $ fromIntegral i --TODO check range

literalFromInt2 :: Int -> IO (V String)
literalFromInt2 i = return $ I $ fromIntegral i --TODO check range

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

data Dev t = Dev !(DevType t) ![Operand t]
    deriving (Show, Eq, Functor)

parseSimpleDevice :: DeviceParser Text (Op String (Operand String))
parseSimpleDevice d
    = case lookup d (devices pure pure) of
        Just dd@(DDesc _name ty _impl)
            -> Right (if length ty > 1 then Mandatory else None
                    , \ops -> parseOp $ fmap unpack $ Dev d ops)
        Nothing -> Left "device type unknown"

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
--     | OpTrap
    deriving (Show, Eq) -- , Functor)

data CmpOp = Lt | Gt | Lte | Gte | Eq | NEq
    deriving (Show, Eq)

--------------------------------------------------------------------------------

mapSimpleOpOperandA
    :: (Applicative m)
    => (CellType -> t -> m n)
    -> Op s t
    -> m (Op s n)
mapSimpleOpOperandA doOperand = f
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
        ("R", [n]   ) -> pure undefined
        ("S", [n]   ) -> pure undefined
        _               -> Left "unknown coil type"
    f (Dev (Contact_ op) arg) = case (fmap toUpper op, arg) of
        (" ", [n]   ) -> pure $ And  n
        ("/", [n]   ) -> pure $ AndN  n
        (">", [a, b]) -> pure $ Cmp Gt a b
        ("<", [a, b]) -> pure $ Cmp Lt a b
        ("P", [n]   ) -> pure $ LdP n
        ("N", [n]   ) -> pure $ LdN n
        _               -> Left "unknown contact type"

--------------------------------------------------------------------------------

type Alloc name = StateT (MemTrack name) (Either String)

-- possibly fetched from config file or pragma
-- ".var "Start" BitWithEdge"
-- type MemoryVariables = [(String, CellType)]

--here vars already have their place in memory
-- type MemoryConfiguration = [(String, CellType, Address Int)]

data Address a = BitAddr a | WordAddr a
    deriving (Show, Eq)

emptyMemory :: MemTrack n
emptyMemory = MemTrack 0 0 M.empty

data MemTrack n = MemTrack
    { bitsSize, wordsSize :: Int
    , variables :: M.Map n (Address Int, CellType)
    }
    deriving (Show)

addCell
    :: Ord n
    => MemTrack n
    -> CellType
    -> n
    -> Alloc n (Address Int, MemTrack n)
addCell mt@MemTrack{..} ty n
    = case M.lookup n variables of
        Just (addr, ty')
            | ty == ty' -> return (addr, mt)
            | otherwise -> throwError $ show ("type mismatch", ty, ty')
        Nothing -> return $ new ty

    where

    updated addr addBits addWords = mt
        { variables = M.insert n (addr, ty) variables
        , wordsSize = wordsSize + addWords
        , bitsSize  = bitsSize + addBits
        }

    new Bit     = let a = BitAddr  bitsSize  in (a, updated a 1 0)
    new TwoBits = let a = BitAddr  bitsSize  in (a, updated a 2 0)
    new Word    = let a = WordAddr wordsSize in (a, updated a 0 1)

--------------------------------------------------------------------------------

emitDevice02
    :: ([Operand (Address Int)], DeviceImpl Word16 Word8)
    -> [Instruction Word16 Word8]
emitDevice02 (ops, impl) = case impl (fmap unAddr ops) of
                            Left err -> error $ show (here, err)
                            Right x -> x
    where
    unAddr :: Operand (Address Int) -> Operand Word8
    unAddr (Var (WordAddr a)) = Var $ fromIntegral a
    unAddr (Var (BitAddr  a)) = Var $ fromIntegral a
    unAddr (Lit _) = undefined -- ???
