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

import Language.Ladder.Interpreter

--------------------------------------------------------------------------------

instructionsToChunks :: [ExtendedInstruction Int Word8 Word16] -> [Chunk]
instructionsToChunks l = foldMap snd l''
    where
    l' = fmap g l

    (_, l'') = Prelude.foldl h (0, []) l'
    h (acc, xs) (chnLen, i) = (acc + chnLen, xs ++ [(acc, i)])

    g (EIJump lbl) = (jmpChunkLen, [C4 1, C12 (fromIntegral (fst (l'' !! lbl)))])
    g (EISimple i) = let s = instructionToChunks i in (sum (fmap chunkLength s), s)

    jmpChunkLen = sum (fmap chunkLength [C4 1, C12 0])

instructionToChunks :: Instruction Word8 Word16 -> [Chunk]
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
    f C4{}  = 1
    f C8{}  = 2
    f C12{} = 3
    f C16{} = 4

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

byteStringToInstructions :: L.ByteString -> [(Int, ExtendedInstruction Int Word8 Word16)]
byteStringToInstructions = runGetL $ runDecode go
    where
    go = do
        empty <- isEmpty
        if empty
        then return []
        else (:) <$> getOneInstruction <*> go

getOneInstruction :: Coding Get (Int, ExtendedInstruction Int Word8 Word16)
getOneInstruction = getBitsFrom 3 (0::Word8) >>= \case
    0  -> return (1, EISimple ITrap)
    1  -> ((1+3,).EIJump)            <$> (fromIntegral <$> getBitsFrom 11 (0::Word16))
    4  -> ((1+1,).EISimple . IPick)  <$> (fromIntegral <$> getBitsFrom 3 (0::Word8))
    6  -> ((1+2,).EISimple . ILdBit) <$> (getBitsFrom 7 (0::Word8))
    12 -> getBitsFrom 3 (0::Word8) >>= \case
        1 -> ((1+1+4,).EISimple . ILdCnA) <$> getBitsFrom 15 (0::Word16)
        _ -> error here
    other -> error $ show (here, other)

-- |Translate input nibble-based labels to index of instruction
findLabels
    :: [(Int, ExtendedInstruction Int Word8 Word16)]
    -> [ExtendedInstruction Int Word8 Word16]
findLabels l = fmap f l
    where
    (_, l' ) = Prelude.foldl h (0, []) l
    h (acc, xs) (chnLen, i) = (acc + chnLen, xs ++ [(acc, i)])

    f (_, EIJump lbl) = case findIndex ((lbl==).fst) l' of
                             Nothing -> error here
                             Just idx -> EIJump idx
    f (_, EISimple i) = EISimple i

--------------------------------------------------------------------------------

-- |List of instruction, for property testing and C stubs generation 
instructionTable
    :: Applicative f
    => f Int
    -> f ca
    -> f a
    -> f w
    -> [f (ExtendedInstruction ca a w)]
instructionTable stk lbl addr lit = 
    [        EIJump <$> lbl
    , pure $ EISimple   ITrap
    , pure $ EISimple   ILdOn
    , pure $ EISimple   IDup
    ,       (EISimple . IPick) <$> stk
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
