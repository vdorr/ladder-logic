{-# OPTIONS_GHC -Wunused-imports -Wall #-}
{-# LANGUAGE CPP, RecordWildCards #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Base16.Lazy as B16

import Data.Word
-- import Data.Foldable
-- import Data.Traversable
import Data.List
import Numeric
-- import Data.List
import Data.Functor.Identity
import Data.Char (toUpper)

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Lazy as M

-- import Tooling
import Language.Ladder.Utils
import Language.Ladder.LadderParser
import Language.Ladder.Interpreter
import Language.Ladder.Target
import Language.Ladder.Simple

import TestUtils

-- http://hackage.haskell.org/package/bits
-- http://hackage.haskell.org/package/haskell-modbus
-- https://github.com/yaacov/ArduinoModbusSlave/blob/master/examples/full/full.ino

--------------------------------------------------------------------------------

asCArray :: L.ByteString -> String
asCArray = intercalate ", " . fmap (("0x"++) . flip showHex "") . L.unpack

--------------------------------------------------------------------------------

allocateMemory1
    :: Cofree (Diagram c (Op s (Operand String)) s') a
    -> Alloc
        String
        (Cofree (Diagram c (Op s (Operand (Address Int))) s') a)
allocateMemory1 = mapDevsM (mapSimpleOpOperandA varFromOperand)

varFromOperand
    :: CellType
    -> Operand String
    -> Alloc String (Operand (Address Int))
varFromOperand Word (Lit v) = return $ Lit v
varFromOperand _    (Lit _) = throwError "type mismatch"
varFromOperand ty   (Var n) = do
    st          <- get
    (addr, st') <- addCell st ty (fmap toUpper n)
    put st'
    return $ Var addr

--------------------------------------------------------------------------------

mapDevsM
    :: Monad m
    => (d -> m d')
    -> Cofree (Diagram c d s) a
    -> m (Cofree (Diagram c d' s) a)
mapDevsM f (p :< n) = (p :<) <$> (mapDevA f n >>= traverse (mapDevsM f))

mapDevA
    :: (Applicative m)
    => (d -> m d')
    -> Diagram c d s a
    -> m (Diagram c d' s a)
mapDevA doDevice = mapDgA pure doDevice pure

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

compileOrDie
    :: FilePath
    -> IO ( MemTrack String
          , [ExtendedInstruction Int Int (Address Int)]
          )
compileOrDie fn = do
    (_pragmas, blocks) <- parseOrDie5 parseSimpleDevice fn
    let doMem ast = runStateT (traverse (traverse allocateMemory1) ast) emptyMemory
    Right (blocks', memory) <- return $ doMem blocks
--     print (here, memory)
    prog <- generateStk2xx return emitBasicDevice literalFromInt blocks'
--     print (here, prog)
    return (memory, prog)

writeBlob
    :: FilePath
    -> [ExtendedInstruction Int Word16 Word8]
    -> IO ()
writeBlob = do
    undefined

aaaargh :: ExtendedInstruction Int Int    (Address Int)
        -> ExtendedInstruction Int Word16  Word8
aaaargh = runIdentity . go
    where
    go (EIJump   lbl) = pure $ EIJump lbl
    go (EISimple i)   = EISimple <$> mapInstruction unAddr fromIntegral i
    go  EIReturn      = pure EIReturn

    unAddr (WordAddr a) = pure $ fromIntegral a
    unAddr (BitAddr  a) = pure $ fromIntegral a

--------------------------------------------------------------------------------

main :: IO ()
main = do
    print here

--     let p = [EISimple $ ILdBit (4::Word8), EISimple $ ILdCnA (0::Word16), EIJump (0::Word16)]
    let p = [EISimple $ ILdBit (4::Word8), EISimple $ ILdCnA (123::Word16), EIJump (1::Int)]
--     let p :: [ExtendedInstruction Int Word8 Word16]
--             = [EISimple $ ILdBit 4, EISimple $ ILdBit 5]
            -- , EISimple $ ILdCnA (0::Word16), EIJump (0::Int)]
    print (here, p)
    print (here, instructionsToChunks p)
    print (here, B16.encode $ chunksToByteString $ instructionsToChunks p)
    print (here
        , findLabels $ byteStringToInstructions
            $ chunksToByteString $ instructionsToChunks p
        )

    args <- getArgs
    print (here, args)
    case args of
        fn : fns -> do
            (memory, prog) <- compileOrDie fn
            print (here, memory)
            print (here, prog)
            let prog' = fmap aaaargh prog
            putStrLn $ asCArray $ programToByteString prog'
            case fns of
                outputFile : _ -> do
                    print (here, outputFile)
                    writeBlob outputFile prog'
                _ -> return ()
        _    ->
            return ()
