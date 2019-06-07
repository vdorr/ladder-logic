{-# OPTIONS_GHC -Wunused-imports #-}

{-# LANGUAGE CPP, TupleSections, TypeSynonymInstances, FlexibleInstances,
    QuasiQuotes, PatternSynonyms,TypeApplications,DeriveAnyClass,
    LambdaCase, ScopedTypeVariables, ViewPatterns, BangPatterns
    , FlexibleContexts #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Data.Bytes.Put
import Data.Bits.Coded
import Data.Bits.Coding
import Data.ByteString.Lazy as L
import Data.Word

-- import Tooling

import Data.ByteString.Base16.Lazy as B16

-- runEncode :: MonadPut m => Coding m () -> m () 
-- encode :: MonadPut m => c -> Coding m () 

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
