{-# OPTIONS_GHC -Wunused-imports -Wall #-}
{-# LANGUAGE CPP, RecordWildCards #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Data.Word
import Data.Functor.Identity
import Data.List

import Language.Ladder.Target
import Language.Ladder.Interpreter

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

    subop is@((op, _ops@(C4 ic : _)) : _) = swcase (show ic) (show op)
            (mkget "subop" (C4 0) ++
            [ unlines $ indentBlock $ switch "subop"
                (fmap (uncurry opcase) (fmap (fmap (drop 1)) is))
            ])
    subop _ = error here

    opcase op (C4 ic : ops) = swcase (show ic) (show op) (argLoads ++ [opCall])
        where
        argNames = zipWith (\i _ -> "f" ++ show i) [0::Integer .. ] ops
        argLoads = concat $ zipWith (\an thisOp -> mkget an thisOp) argNames ops
        opCall = mkopstub op ++ "(" ++ intercalate ", " ("p" : argNames) ++ ");"
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

--------------------------------------------------------------------------------

main :: IO ()
main = do
--     print here
--     print (here, instructionsToChunks i0)
    putStrLn cstub1
