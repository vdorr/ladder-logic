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

import Control.Monad.Except hiding (fail)

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

runLadderParser
    :: DeviceParser t d
    -> LdP d t a
    -> [(Int, [((Int, Int), Tok t)])]
    -> Either String (a, Dg (Tok t))
runLadderParser = runParser

--------------------------------------------------------------------------------

type DeviceParser name dev = DevType name
    -> Either String
        ( DevOpFlag
        , [Operand name] -> Either String dev
        )

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

literalFromInt :: (MonadError String m, Monad m, Bounded a, Integral a) => Int -> m a
literalFromInt i = return $ fromIntegral i --TODO check range

literalFromInt2 :: (MonadError String m, Monad m) => Int -> m (V String)
literalFromInt2 i = return $ I $ fromIntegral i --TODO check range

generateStk2xx
    :: (Show addr, Show word, Show lbl, Eq lbl, MonadError String m, Monad m)
    => (dev -> Either String x) --TODO swap (Either String) for m
    -> (x -> [Instruction word addr])
    -> (Int -> m word)
    -> [(Maybe lbl, Cofree (Diagram Void dev lbl) DgExt)]
    -> m [ExtendedInstruction Int word addr]
generateStk2xx doOp emitDev literalFromInt ast = do
    ast'   <- for ast (traverse (mapOpsM (liftEither . doOp))) --FIXME liftEither
    ast''        <- for ast' (traverse (generateStk2' literalFromInt emitDev))
    ast''' <- case resolveLabels ast'' of
                   Left err -> throwError err
                   Right x -> return x
    return ast'''

mapOpsM
    :: Monad m => (a -> m b)
    -> Cofree (Diagram c a s) p
    -> m (Cofree (Diagram c b s) p)
mapOpsM f (a :< n) = (a :<) <$> (mapDgA pure f pure n >>= traverse (mapOpsM f))

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
