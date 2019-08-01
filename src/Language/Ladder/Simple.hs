{-# LANGUAGE OverloadedStrings #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Simple where

import Data.Traversable
import Data.Text (Text)
import Data.Void

import Control.Monad.Except

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
        Just (DDesc _name ty impl)
            -> Right (if length ty > 1 then Mandatory else None
                    , \ops -> Right (zip (fmap snd ty) ops, impl))
        Nothing -> Left "device type unknown"

--------------------------------------------------------------------------------

literalFromInt2 :: (MonadError String m, Monad m) => Int -> m (V String)
literalFromInt2 i = return $ I $ fromIntegral i --TODO check range

generateStk2xx
    :: (Show addr, Show word, Show lbl, Eq lbl, MonadError String m, Monad m)
    => (dev -> Either String x) --TODO swap (Either String) for m
    -> (x -> [Instruction word addr])
    -> (Int -> m word)
    -> [(Maybe lbl, Cofree (Diagram Void dev lbl) DgExt)]
    -> m [ExtendedInstruction Int word addr]
generateStk2xx doOp emitDev litFromInt ast = do
    ast'   <- for ast (traverse (mapOpsM (liftEither . doOp))) --FIXME liftEither
    ast''  <- for ast' (traverse (generateStk2' litFromInt emitDev))
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
