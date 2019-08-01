#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Target where

import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bits.Coded
import Data.Bits.Coding
import qualified Data.ByteString.Lazy as L
-- import Data.ByteString.Base16.Lazy as B16
import Data.Binary.Get (Get)

import Data.Word
import Data.List
import Data.Foldable
-- import Data.Traversable
import qualified Data.Map.Lazy as M
-- import Data.Word
import Control.Monad.State
import Control.Monad.Except hiding (fail)

import Language.Ladder.Interpreter
import Language.Ladder.LadderParser

--------------------------------------------------------------------------------

--XXX still needed?
mapInstruction :: Applicative f =>
                        (t1 -> f a)
                        -> (t2 -> f w) -> Instruction t2 t1 -> f (Instruction w a)
mapInstruction g h = f
    where
    f ITrap = pure ITrap

    f ILdOn = pure ILdOn
    f IDup = pure IDup
    f (IPick lbl) = pure $ IPick lbl
    f IDrop = pure IDrop

    f (ILdBit a) = ILdBit <$> g a
    f (IStBit a) = IStBit <$> g a

    f IAnd = pure IAnd
    f IOr = pure IOr
    f INot = pure INot

    f (ILdCnA w) = ILdCnA <$> h w
    f ILdM = pure ILdM
    f IStM = pure IStM

    f IEq = pure IEq
    f ILt = pure ILt
    f IGt = pure IGt

--------------------------------------------------------------------------------

programToByteString :: [ExtendedInstruction Int Word16 Word8] -> L.ByteString
programToByteString = chunksToByteString . instructionsToChunks

instructionsToChunks :: [ExtendedInstruction Int Word16 Word8] -> [Chunk]
instructionsToChunks l = foldMap snd l''
    where
    l' = fmap g l

    (_, l'') = Prelude.foldl h (0, []) l'
    h (acc, xs) (chnLen, i) = (acc + chnLen, xs ++ [(acc, i)])

    g (EIJump lbl) = (jumpInstructionLength, [C4 1, C12 (fromIntegral (fst (l'' !! lbl)))])
    g  EIReturn = (1, [C4 13])
    g (EISimple i) = let s = instructionToChunks i in (sum (fmap chunkLength s), s)

    jumpInstructionLength = sum (fmap chunkLength [C4 1, C12 0])


instructionToChunks :: Instruction Word16 Word8 -> [Chunk]
instructionToChunks = f
    where
    f  ITrap     = [C4 0]
    f  ILdOn     = [C4 2]
    f  IDup      = [C4 3]
    f (IPick  i) = [C4 4, C4 (fromIntegral i)] --FIXME check range!!
    f  IDrop     = [C4 5]
    f (ILdBit a) = [C4 6, C8 (fromIntegral a)]
    f (IStBit a) = [C4 7, C8 (fromIntegral a)]
    f  IAnd      = [C4 8]
    f  IOr       = [C4 10]
    f  INot      = [C4 11]
--     | IXor
    f (ILdCnA w) = [C4 12, C4 1, C16 (fromIntegral w)]
    f  ILdM      = [C4 12, C4 2]
    f  IStM      = [C4 12, C4 3]
--     | IOp Operator --instead of lt,gt,etc
    f  IEq       = [C4 12, C4 4]
    f  ILt       = [C4 12, C4 5]
    f  IGt       = [C4 12, C4 6]

-- |Return length of chunk in nibbles
chunkLength :: Chunk -> Integer
chunkLength = f
    where
    f C4  {} = 1
    f C8  {} = 2
    f C12 {} = 3
    f C16 {} = 4

-- |Some bits
data Chunk
    = C4  Word8 -- ^lower four bits are used
    | C8  Word8
    | C12 Word16 -- ^lower twelwe bits are used
    | C16 Word16
    deriving Show

chunksToByteString :: Foldable t => t Chunk -> L.ByteString
chunksToByteString l
    = runPutL $ runEncode $ do
        for_ l $ \case
            C4  x -> putBitsFrom 3  x
            C8  x -> putBitsFrom 7  x
            C12 x -> putBitsFrom 11 x
            C16 x -> putBitsFrom 15 x
        flush

byteStringToInstructions :: L.ByteString -> [(Int, ExtendedInstruction Int Word16 Word8)]
byteStringToInstructions = runGetL $ runDecode go
    where
    go = do
        empty <- noMoreBits
        if empty
        then return []
        else (:) <$> getOneInstruction <*> go

noMoreBits :: Coding Get Bool
noMoreBits = Coding $ \k i b -> do
    empty <- isEmpty
    k (empty && i == 0) i b

getOneInstruction :: Coding Get (Int, ExtendedInstruction Int Word16 Word8)
getOneInstruction = get4 >>= \case
    0  -> return (1,        EISimple   ITrap)
    1  ->       ((1 + 3,) . EIJump   . fromIntegral)            <$> get12
    2  -> return (1,        EISimple   ILdOn)
    3  -> return (1,        EISimple   IDup)
    4  ->       ((1 + 1,) . EISimple . IPick . fromIntegral)  <$> get4
    5  -> return (1,        EISimple   IDrop)
    6  ->       ((1 + 2,) . EISimple . ILdBit) <$> get8
    7  ->       ((1 + 2,) . EISimple . IStBit) <$> get8
    8  -> return (1,        EISimple   IAnd)
    10 -> return (1,        EISimple   IOr)
    11 -> return (1,        EISimple   INot)
    12 -> get4 >>= \case
        1 ->       ((1 + 1 + 4,) .  EISimple . ILdCnA) <$> get16
        2 -> return (1 + 1,         EISimple   ILdM)
        3 -> return (1 + 1,         EISimple   IStM)
        4 -> return (1 + 1,         EISimple   IEq)
        5 -> return (1 + 1,         EISimple   ILt)
        6 -> return (1 + 1,         EISimple   IGt)
        other -> error $ show (here, other)
    13 -> return (1,        EIReturn)
    other -> error $ show (here, other)

    where

    get4 = getBitsFrom 3 (0::Word8)
    get8 = getBitsFrom 7 (0::Word8)
    get12 = getBitsFrom 11 (0::Word16)
    get16 = getBitsFrom 15 (0::Word16)


-- |Translate input nibble-based labels to index of instruction
findLabels
    :: [(Int, ExtendedInstruction Int Word16 Word8)]
    -> [ExtendedInstruction Int Word16 Word8]
findLabels l = fmap f l
    where
    (_, l' ) = Prelude.foldl h (0, []) l
    h (acc, xs) (chnLen, i) = (acc + chnLen, xs ++ [(acc, i)])

    f (_, EIJump lbl) = case findIndex ((lbl==).fst) l' of
                             Nothing -> error here
                             Just idx -> EIJump idx
    f (_, EIReturn)   = EIReturn
    f (_, EISimple i) = EISimple i

--------------------------------------------------------------------------------

-- |List of instruction, for property testing and C stubs generation 
instructionTable
    :: Applicative f
    => f Int
    -> f lbl
    -> f a
    -> f w
    -> [f (ExtendedInstruction lbl w a)]
instructionTable stk lbl addr lit = 
    [        EIJump <$> lbl
    , pure   EIReturn
    , pure $ EISimple   ITrap
    , pure $ EISimple   ILdOn
    , pure $ EISimple   IDup
    ,       (EISimple . IPick)  <$> stk
    , pure $ EISimple   IDrop
    ,       (EISimple . ILdBit) <$> addr
    ,       (EISimple . IStBit) <$> addr
    , pure $ EISimple   IAnd
    , pure $ EISimple   IOr
    , pure $ EISimple   INot
    ,       (EISimple . ILdCnA) <$> lit
    , pure $ EISimple   ILdM
    , pure $ EISimple   IStM
    , pure $ EISimple   IEq
    , pure $ EISimple   ILt
    , pure $ EISimple   IGt
    ]

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

literalFromInt :: (MonadError String m, Monad m, Bounded a, Integral a) => Int -> m a
literalFromInt i = return $ fromIntegral i --TODO check range

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
