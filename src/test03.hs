{-# OPTIONS_GHC -Wunused-imports #-}

{-# LANGUAGE CPP, TupleSections, TypeSynonymInstances,
    FlexibleInstances, PatternSynonyms, LambdaCase,
    ScopedTypeVariables, FlexibleContexts, RecordWildCards #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import System.Environment (getArgs)

-- import Data.Bytes.Put
-- import Data.Bytes.Get
-- import Data.Bits.Coded
-- import Data.Bits.Coding
-- import Data.Binary.Get (Get)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Base16.Lazy as B16

import Data.Word
import Data.Foldable
import Data.Traversable
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
import Language.Ladder.DiagramParser hiding (get, put)
import Language.Ladder.LadderParser
import Language.Ladder.Interpreter
import Language.Ladder.Target


import TestUtils

-- http://hackage.haskell.org/package/bits
-- http://hackage.haskell.org/package/haskell-modbus
-- https://github.com/yaacov/ArduinoModbusSlave/blob/master/examples/full/full.ino

--------------------------------------------------------------------------------

i0 :: [ExtendedInstruction Int Word16 Word8]
i0 = runIdentity <$> instructionTable (pure 0) (pure 0) (pure 0) (pure 0)

cstub1 :: String
cstub1 = unlines (concat (concat q))
    where

    l = fmap (\i -> (i, instructionsToChunks [i])) i0
    l' = groupBy (\(_, C4 a : _) (_, C4 b : _) -> a == b) l
    (_stubs, q) = unzip $ fmap f l'

    f []         = error here
    f [(op, ch)] = ([], [ opcase op ch ])
    f is         = ([], [ subop is ])

    subop is@((op, ops@(C4 ic : _)) : _) = swcase (show ic) (show op)
            (mkget "subop" (C4 0) ++
            [ unlines $ indentBlock $ switch "subop"
                (fmap (uncurry opcase) (fmap (fmap (drop 1)) is))
            ])

    opcase op (C4 ic : ops) = swcase (show ic) (show op) (argLoads ++ [opCall])
        where
        argNames = zipWith (\i _ -> "f" ++ show i) [0..] ops
        argLoads = concat $ zipWith (\an op -> mkget an op) argNames ops
        opCall = mkopstub op ++ "(" ++ intercalate ", " argNames ++ ");"
    opcase _ _ = error here

    switch val cases =
        [ "switch ( " ++ val ++ " ) {" ]
        ++ fmap (unlines . indentBlock) cases ++
        [ "}" ]

    swcase val comment body =
        [ "case " ++ val ++ " : { //" ++ comment ]
        ++ indentBlock (body ++ ["break;"]) ++
        [ "}" ]

    indentBlock = fmap ("    "++)

--     mkget n C4 {} = ["const uint8_t " ++ n ++ " = get4(blob, addr);"];
--     mkget n C8 {} = ["const uint8_t " ++ n ++ " = get8(blob, addr);"];
--     mkget n C12{} = ["const uint16_t " ++ n ++ " = get12(blob, addr);"];
--     mkget n C16{} = ["const uint16_t " ++ n ++ " = get12(blob, addr);"];

    mkget n C4 {} = ["uint8_t " ++ n ++ ";", "get4(blob, addr, " ++ n ++ ");"];
    mkget n C8 {} = ["uint8_t " ++ n ++ ";", "get8(blob, addr, " ++ n ++ ");"];
    mkget n C12{} = ["uint16_t " ++ n ++ ";", "get12(blob, addr, " ++ n ++ ");"];
    mkget n C16{} = ["uint16_t " ++ n ++ ";", "get12(blob, addr, " ++ n ++ ");"];

    mkopstub (EISimple op) = "op_" ++ name
        where
        name = head $ words $ show op
    mkopstub (EIJump _) = "op_jump"
    mkopstub  EIReturn = "op_return"

asCArray :: L.ByteString -> String
asCArray = intercalate ", " . fmap (("0x"++) . flip showHex "") . L.unpack

--------------------------------------------------------------------------------

-- possibly fetched from config file or pragma
-- ".var "Start" BitWithEdge"
type MemoryVariables = [(String, CellType)]

--here vars already have their place in memory
type MemoryConfiguration = [(String, CellType, Address Int)]

-- data CellAddress a = Bits Int | Word Int
zgh :: MemoryConfiguration -> [(String, CellType, Int)]
zgh = undefined

data Address a = BitAddr a | WordAddr a
    deriving (Show, Eq)

-- data Operand' = Mem Address | Lit' Int
--     deriving (Show, Eq)

-- data Dev' = Dev' String [Operand']
--     deriving (Show, Eq)

-- assignAddresses
--     :: Cofree (Diagram () Dev String) DgExt
--     -> MemoryConfiguration
--     -> Either String (Cofree (Diagram () Dev' String) DgExt)
-- assignAddresses = undefined

-- allocateMemory :: MemoryVariables -> MemoryConfiguration
-- allocateMemory = undefined

--evil approach
extractVariables
    :: Cofree (Diagram () (Op (Operand String) String) String) DgExt
    -> MemoryVariables
extractVariables = undefined
-- data Dev = Dev String [Operand]

--XXX do two passes
--gather (+allocate) and assign addresses
--that way is more like usual setting where vars are declared first
xxxx ast = flip runStateT (0, M.empty, 0, M.empty) $ mapDgA pure f pure ast
    where
    f (And    a  ) = And <$> doOperand Bit a
    f (AndN   a  ) = undefined
    f (St     a  ) = undefined
    f (StN    a  ) = undefined
    f (Cmp op a b) = Cmp op <$> doOperand Word a <*> doOperand Word b
    f other        = pure other

    doOperand _t (Lit _) = throwError "TODO"
    doOperand t (Var n) = doVariable t n

    doVariable t n
        = throwError "TODO"
--         = do
--             st <- get
--             runStateT ( alterF (alterVar t) )

    alterVar t (Just (t', _addr))
        | t == t'   = undefined
        | otherwise = throwError "type mismatch"
    alterVar t Nothing = undefined


emptyMemory = MemTrack 0 0 M.empty

allocateMemory
    :: Cofree (Diagram c' (Op s (Operand String)) s') a
    -> StateT
        (MemTrack String)
        (Either String)
        (Cofree (Diagram c' (Op s (Operand (Address Int))) s') a)
allocateMemory (p :< n)
    = (p :<) <$> (hjdtfd doOperand n >>= traverse allocateMemory)
    where
    doOperand Word (Lit v) = return $ Lit v
    doOperand _    (Lit _) = throwError "type mismatch"
    doOperand ty   (Var n) = do
        st          <- get
        (addr, st') <- addCell st ty n
        put st'
        return $ Var addr

-- xdgcfg :: _
xdgcfg doOperand st ast = flip runStateT st $ mapDgA pure (cfggjhj doOperand) pure ast
hjdtfd doOperand ast = mapDgA pure (cfggjhj doOperand) pure ast

cfggjhj doOperand = f
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

addCell :: MemTrack String
                      -> CellType
                      -> String
                      -> StateT
                           (MemTrack String)
                           (Either String)
                           (Address Int, MemTrack String)
addCell mt@MemTrack{..} ty n0
    = case M.lookup n variables of
        Just (addr, ty')
            | ty == ty' -> return (addr, mt)
            | otherwise -> throwError $ show ("type mismatch", ty, ty')
        Nothing -> return $ new ty

    where
    n = fmap toUpper n0

    updated addr addBits addWords = mt
        { variables = M.insert n (addr, ty) variables
        , wordsSize = wordsSize + addWords
        , bitsSize  = bitsSize + addBits
        }

    new Bit     = let a = BitAddr  bitsSize  in (a, updated a 1 0)
    new TwoBits = let a = BitAddr  bitsSize  in (a, updated a 2 0)
    new Word    = let a = WordAddr wordsSize in (a, updated a 0 1)

data MemTrack n = MemTrack
    { bitsSize, wordsSize :: Int
    , variables :: M.Map n (Address Int, CellType)
    }
    deriving (Show)

--------------------------------------------------------------------------------

literalFromInt :: (Bounded a, Integral a) => Int -> IO a
literalFromInt i = return $ fromIntegral i --TODO check range

compileOrDie
    :: FilePath
    -> IO ( MemTrack String
          , [ExtendedInstruction Int Int (Address Int)]
          )
compileOrDie fn = do
    (_pragmas, blocks) <- parseOrDie5 fn

    let Right (blocks', memory)
                = runStateT (traverse (traverse allocateMemory) blocks) emptyMemory
--     print (here, memory)
    prog <- generateStk2xx literalFromInt blocks'
--     print (here, prog)
    return (memory, prog)

writeBlob
    :: FilePath
    -> [ExtendedInstruction Int Word16 Word8]
    -> IO ()
writeBlob = do
    undefined

--------------------------------------------------------------------------------

aaaargh :: ExtendedInstruction Int Int    (Address Int)
        -> ExtendedInstruction Int Word16  Word8
aaaargh = runIdentity . go
    where
    go (EIJump lbl) = pure $ EIJump lbl
    go (EISimple i) = EISimple <$> mapInstruction unAddr fromIntegral i
    go  EIReturn    = pure EIReturn

    unAddr (WordAddr a) = pure $ fromIntegral a
    unAddr (BitAddr  a) = pure $ fromIntegral a

main :: IO ()
main = do
    print here
--     let x = runPutL $ runEncode
--             $ do
--     --             putUnaligned $ 
--                 putBitsFrom 3 (11 :: Word8)
--                 putBitsFrom 11 (3800 :: Word16)
--     --             putBitsFrom 6 (0x00 :: Word8)
--                 flush
-- 
--     print (here, x, L.unpack x, B16.encode x)
-- 
--     let y = runGetL (runDecode $ do
--         getBitsFrom 7 (0::Word8)
-- --         return ()
--         ) (L.pack [0xaa,0x55])
-- 
--     print (here, y)

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
    print (here, instructionsToChunks i0)
--     putStrLn cstub
    putStrLn cstub1

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
