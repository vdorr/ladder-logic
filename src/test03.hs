{-# OPTIONS_GHC -Wunused-imports #-}

{-# LANGUAGE CPP, TupleSections, TypeSynonymInstances, FlexibleInstances,
    QuasiQuotes, PatternSynonyms,TypeApplications,DeriveAnyClass,
    LambdaCase, ScopedTypeVariables, ViewPatterns, BangPatterns
    , FlexibleContexts #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bits.Coded
import Data.Bits.Coding
import Data.ByteString.Lazy as L
import Data.Word
import Data.Foldable

import Tooling

import Data.ByteString.Base16.Lazy as B16

-- runEncode :: MonadPut m => Coding m () -> m () 
-- encode :: MonadPut m => c -> Coding m () 

type LabelIn = Int
type AddressIn = Int
type ConstWordIn = Int

-- xxx
--     :: [ExtendedInstruction LabelIn AddressIn ConstWordIn]
--     -> Either String [Chunk]
-- xxx l = Right $ foldMap snd l''
-- xxx
--     :: [ExtendedInstruction LabelIn AddressIn ConstWordIn]
--     -> [Chunk]
xxx l = foldMap snd l''
    where
    l' = fmap g l

    (_, l'') = Prelude.foldl h (0, []) l'
    h (acc, xs) (chnLen, i) = (acc + chnLen, xs ++ [(acc, i)])

    g (EIJump lbl) = (jmpChunkLen, [C4 1, C12 (fromIntegral (fst (l'' !! lbl)))])
    g (EISimple i) = let s = f i in (sum (fmap chunkLength s), s)

    jmpChunkLen = sum (fmap chunkLength [C4 1, C12 0])

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


chunkLength = f
    where
    f C4{}  = 1
    f C8{}  = 2
    f C12{} = 3
    f C16{} = 4

data Chunk
    = C4 Word8
    | C8 Word8
    | C12 Word16
    | C16 Word16
    deriving Show

xxy :: Foldable t => t Chunk -> L.ByteString
xxy l
    = runPutL $ runEncode $ do
        for_ l $ \case
            C4  x -> putBitsFrom 3  x
            C8  x -> putBitsFrom 7  x
            C12 x -> putBitsFrom 11 x
            C16 x -> putBitsFrom 15 x
        flush

xxz :: L.ByteString -> [ExtendedInstruction Int Word8 Word16]
xxz = runGetL $ runDecode $ do
    f
    where
    f = do
        empty <- isEmpty
        if empty
        then return []
        else (:) <$> getOne <*> f

    getOne = getBitsFrom 3 (0::Word8) >>= \case
        0  -> return               $ EISimple $ ITrap
        1  -> EIJump              <$> (fromIntegral <$> getBitsFrom 11 (0::Word16))
        4  -> (EISimple . IPick)  <$> (fromIntegral <$> getBitsFrom 3 (0::Word8))
        6  -> (EISimple . ILdBit) <$> (getBitsFrom 7 (0::Word8))
        12 -> getBitsFrom 3 (0::Word8) >>= \case
            1 -> (EISimple . ILdCnA) <$> getBitsFrom 15 (0::Word16)
            _ -> error here
        other -> error $ show (here, other)

main :: IO ()
main = do
    print here
    let x = runPutL $ runEncode
            $ do
    --             putUnaligned $ 
                putBitsFrom 3 (11 :: Word8)
                putBitsFrom 11 (3800 :: Word16)
    --             putBitsFrom 6 (0x00 :: Word8)
                flush

    print (here, x, L.unpack x, B16.encode x)

    let y = runGetL (runDecode $ do
        getBitsFrom 7 (0::Word8)
--         return ()
        ) (L.pack [0xaa,0x55])

    print (here, y)
    
--     let p = [EISimple $ ILdBit (4::Word8), EISimple $ ILdCnA (0::Word16), EIJump (0::Word16)]
    let p = [EISimple $ ILdBit (4::Word8), EISimple $ ILdCnA (123::Word16), EIJump (1::Int)]
--     let p :: [ExtendedInstruction Int Word8 Word16]
--             = [EISimple $ ILdBit 4, EISimple $ ILdBit 5]
            -- , EISimple $ ILdCnA (0::Word16), EIJump (0::Int)]
    print (here, p)
    print (here, xxx p)
    print (here, B16.encode $ xxy $ xxx p)
    print (here, xxz $ xxy $ xxx p)

