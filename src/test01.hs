{-# OPTIONS_GHC -Wunused-imports  -Wall #-}
{-# LANGUAGE CPP, TupleSections, FlexibleContexts, ScopedTypeVariables
    , BlockArguments
    , OverloadedStrings, LambdaCase
#-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Foldable
import Data.Void

import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Simple
import Language.Ladder.Interpreter
import Language.Ladder.Types
import Language.Ladder.Utils
import Language.Ladder.Eval

import Control.Monad.State
import Control.Monad.Except
-- import Data.Bifunctor
-- import Data.List
import Data.Text (Text, unpack)
import qualified Data.Text as T --(Text, lines)
import Text.Printf
import Data.Traversable

import Data.Bits

-- import System.Console.ANSI.Types
-- import System.Console.ANSI.Codes

import TestUtils

--------------------------------------------------------------------------------

data AI r w a
    = AITrap --invoke debugger
    | AILdOn
    | AILdReg !r
    | AIStReg !r

    | AILdBit !a
    | AIStBit !a

    | AIAnd   !r
    | AIOr    !r
    | AINot

    | AILdCnA !w
    | AILdM
    | AIStM

    | AIEq
    | AILt
    | AIGt
    deriving (Show, Eq)

encAccI = f
    where
    byte v = Left ""
    word v = Left ""
    f  AITrap         = byte 0x01
    f  AILdOn         = byte 0x02
    f (AILdReg (R r)) = byte (0x80 .|. r)
    f (AILdReg (M a)) = undefined --byte (0x80 .|. a)
    f (AIStReg (R r)) = byte (0xc0 .|. r)
    f (AIStReg (M a)) = undefined --byte (0xc0 .|. a)

    f (AILdBit    a ) = byte 0x03 *> byte a
    f (AIStBit    a ) = byte 0x04 *> byte a

    f (AIAnd   (R r)) = byte 0x05 *> byte r
    f (AIAnd   (M a)) = byte 0x05 *> byte a
    f (AIOr    (R r)) = byte 0x06 *> byte r
    f (AIOr    (M a)) = byte 0x06 *> byte a
    f  AINot          = byte 0x07

    f (AILdCnA    w ) = byte 0x08 *> word w
    f  AILdM          = byte 0x09
    f  AIStM          = byte 0x0a

    f  AIEq           = byte 0x0b
    f  AILt           = byte 0x0c
    f  AIGt           = byte 0x0d

    
--------------------------------------------------------------------------------

type AccItpSt a = (Bool, [Bool], [V a], Memory a)

-- accEval :: (Eq address, Show address)
--      => AI Int (V address) address
--      -> AccItpSt address
--      -> Either
--          (AccItpSt address, String)
--          (AccItpSt address)
-- accEval = go
--     where
--     go  AITrap     st                = halt st "trap"
--     go  AILdOn        (a, rf, os, m) = pure (True, rf, os, m)
-- 
--     go (AILdReg r) st@(_, rf, os, m) = (,rf,os,m) <$> getReg r st
-- 
--     go (AIStReg r) st@(a, rf, os, m)
--         | (f,v:g) <- splitAt r rf    = pure (a, f ++ a : g, os, m)
--         | otherwise                  = halt st (show ("rf idx out of range"::String, r, rf))
--     go (AILdBit i) st@(_, rf, os, m)
--         | Just (X v) <- lookup i m   = pure (v, rf, os, m)
--         | otherwise                  = halt st (show (here, "invalid memory access"::String, i))
--     go (AIStBit i) st@(a, rf, os, m)
--         | (m0,(_,X _):m1) <- break ((i==) . fst) m
--                                      = pure (a, rf, os, (m0 ++ (i, X a) : m1))
--         | otherwise                  = halt st (show (here, "invalid memory access"::String, i))
-- 
-- --     go (AIAnd   r) st@(a, rf, os, m) = getReg r st >>= \b -> pure (a && b, rf, os, m)
--     go (AIAnd   r) st@(a, rf, os, m) = (,rf,os,m) <$> ((a &&) <$> getReg r st)
-- 
--     go (AIOr    r) st@(a, rf, os, m) = (,rf,os,m) <$> ((a ||) <$> getReg r st)
--     go  AINot      st@(a, rf, os, m) = pure (not a, rf, os, m)
-- 
--     go (AILdCnA k) (a, rf, os, m)    = pure (a, rf, k : os, m)
--     go  AILdM      st@(a, rf, A i : os, m)
--        | Just v <- lookup i m        = pure (a, rf, v : os, m)
--        | otherwise                   = halt st (show (here, "invalid memory access"::String, i))
--     go  AIStM      st = undefined --TODO
--     go  AIEq       st = undefined
--     go  AILt       (_, rf,   I b : I a : os, m) = pure (a < b, rf, os, m)
--     go  AIGt       (_, rf,   I b : I a : os, m) = pure (a > b, rf, os, m)
--     go  i          st                = halt st (show i)
-- 
--     halt st e = Left (st, show e)
-- 
--     getReg r st@(a, rf, os, m)
-- --         | r >= 0 && r < length rf = pure (rf !! r, rf, os, m)
--         | r >= 0 && r < length rf = pure (rf !! r)
--         | otherwise               = Left (st, show ("rf idx out of range"::String, r, rf))

--------------------------------------------------------------------------------

type AccItpSt2 a = (Bool, [V a], Memory a)

accEval2 :: (Eq address, Show address)
     => AI address (V address) address
     -> AccItpSt2 address
     -> Either
         (AccItpSt2 address, String)
         (AccItpSt2 address)
accEval2 = go
    where
    go  AITrap     st                = halt st ("trap"::String)
    go  AILdOn        (_a, os, m) = pure (True, os, m)

    go (AILdReg r) st@(_, os, m) = (,os,m) <$> getReg r st

--     go (AIStReg r) st@(a, os, m)
--         | (f,v:g) <- splitAt r rf    = pure (a, f ++ a : g, os, m)
--         | otherwise                  = halt st (show ("rf idx out of range"::String, r, rf))
    go (AIStReg r) st = storeBit r st
--     go (AILdBit i) st@(_, os, m)
--         | Just (X v) <- lookup i m   = pure (v, os, m)
--         | otherwise                  = halt st (show (here, "invalid memory access"::String, i))
    go (AILdBit i) st@(_a, os, m) = (,os,m) <$> loadBit i st
--     go (AIStBit i) st@(a, os, m)
--         | (m0,(_,X _):m1) <- break ((i==) . fst) m
--                                      = pure (a, os, (m0 ++ (i, X a) : m1))
--         | otherwise                  = halt st (show (here, "invalid memory access"::String, i))
    go (AIStBit i) st = storeBit i st
    go (AIAnd   r) st@(a, os, m) = (,os,m) <$> ((a &&) <$> getReg r st)
    go (AIOr    r) st@(a, os, m) = (,os,m) <$> ((a ||) <$> getReg r st)
    go  AINot      _st@(a, os, m) = pure (not a, os, m)

    go (AILdCnA k) (a, os, m)    = pure (a, k : os, m)
    go  AILdM      st@(a, A i : os, m)
       | Just v <- lookup i m        = pure (a, v : os, m)
       | otherwise                   = halt st (show (here, "invalid memory access"::String, i))
    go  AIStM      _st = undefined --TODO
    go  AIEq       _st = undefined
    go  AILt       (_,    I b : I a : os, m) = pure (a < b, os, m)
    go  AIGt       (_,    I b : I a : os, m) = pure (a > b, os, m)
    go  i          st                = halt st (show i)

    halt st e = Left (st, show e)

    storeBit i st@(a, os, m)
        | (m0,(_,X _):m1) <- break ((i==) . fst) m
                                     = pure (a, os, (m0 ++ (i, X a) : m1))
        | otherwise                  = halt st (show (here, "invalid memory access"::String, i))

    loadBit i st@(_, _, m)
        | Just (X v) <- lookup i m   = pure v
        | otherwise                  = halt st (show (here, "invalid memory access"::String, i))
    getReg = loadBit
--     getReg r st@(a, os, m)
--         = undefined
--         | r >= 0 && r < length rf = pure (rf !! r)
--         | otherwise               = Left (st, show ("rf idx out of range"::String, r, rf))

--------------------------------------------------------------------------------

data Reg adr = R !Int | M !adr
    deriving Show

accuEmit
    :: (Eq p, Show p, Show l, Monad m)
    => (d -> m [ExtendedInstruction l (AI (Reg adr) k adr)])
    -> p
    -> p
    -> Diagram c d l (Cofree (Diagram c d l) p)
    -> StateT
        (AccuEmitState p [(Maybe p, ExtendedInstruction l (AI (Reg adr) k adr))])
        m -- (Either String)
        p
accuEmit emitDevice q p x = go x *> pure p
    where

    go (Node    []  ) = accSet p -- "optimization"
    go (Node    w   ) = currentNodeDependencies w
                      >>= \deps
                      -> getValue q
                      *> (for_ deps \d
                          -> getFromRegs d
                          >>= \r -> accEmit' [ EISimple $ AIOr $ R r ])
                      *> accSet p
                      *> (accSpill (length w) p) --cant be empty unless (null w) 
    go  Sink          =   gets (elem p . aesNodes) 
                      >>= \case
                                True -> accSet p
                                     *> accSpill 1 p
                                _    -> return ()
    go (Source _a   ) =  accEmit' [ EISimple AILdOn ]
                      *> accSet p
    go  End           =  return ()
    go (Device  d _a) =  getValue q
                      *> (lift (emitDevice d) >>= accEmit')
                      *> accSet p
    go (Jump    l   ) =  getValue q
                      *> accEmit' [ EIJump l ]
    go (Cont   _c _a) =  undefined
    go (Conn   _c   ) =  undefined

    currentNodeDependencies w
        = gets aesSinks
        >>= \sinks -> return $ foldMap (checkDataDep sinks) w ++ []

    --find value in registers
    getValue pp = do
        accu <- gets aesAccu
        if accu == pp
        then return ()
        else getFromRegs pp >>= load

    accEmit' = accEmit . fmap (Just p,)

    load r = accEmit' [ EISimple $ AILdReg $ R r ]

    getFromRegs pp = do
        rf <- gets aesRegisters
        case break ((==pp) . fst) rf of
            (_pre, []) -> undefined --not found
            (pre, (ppp, (v, uses)) : rest) -> do
                let rf' = case uses of
                            UseCount 1 -> pre ++ rest
                            UseCount u -> pre ++ (ppp, (v, UseCount $ u - 1)) : rest
                            _ -> undefined --TODO
                modify \st -> st { aesRegisters = rf'}
                return v

accEmit :: MonadState (AccuEmitState p [a]) m => [a] -> m ()
accEmit s = modify \st -> st { aesCode = aesCode st ++ s }

accSet :: MonadState (AccuEmitState p w) m => p -> m ()
accSet q = modify \st -> st { aesAccu = q }

accSpill
    :: Monad m
    => Int
    -> p
    -> StateT (AccuEmitState p [(Maybe p, ExtendedInstruction l (AI (Reg adr) k adr))]) m ()
accSpill uses rename = do
    rf   <- gets aesRegisters

    let rUseNumber = snd . snd
    case break ((UseCount 0==) . rUseNumber) rf of
        (_pre, []) -> do --free register not found
            modify \st -> st { aesRegisters =  rf ++ [(rename, (length rf, UseCount uses))] }
            accEmit [ (Nothing, EISimple $ AIStReg (R $ length rf)) ] 
        (pre, (_ppp, (v, _uses)) : rest) -> do
            modify \st -> st { aesRegisters =  pre ++ (rename, (v, UseCount uses)) : rest }
            accEmit [ (Nothing, EISimple $ AIStReg $ R v) ] 

accuPost
    :: Monad m
    => p
    -> Cofree (Diagram continuation device label) a
    -> StateT (AccuEmitState p [(Maybe p, ExtendedInstruction l (AI (Reg adr) k adr))]) m ()
accuPost q (_ :< Node w@(_:_)) = accSpill (length w) q
accuPost _  _                  = return () --not needed, forget it


data SlotUse = UseCount Int | Forever
    deriving Eq

data AccuEmitState p w = AccuEmitState
    { aesAccu      :: p
    , aesSinks     :: [p]
    , aesNodes     :: [p]
    , aesRegisters :: [(p, (Int, SlotUse))]
    , aesCode      :: w
--     , aesRCount    :: Int --number of registers required
    }

blargh
    :: (Ord p, Show p, Show l, Monad m)
    => (d -> m [ExtendedInstruction l (AI (Reg adr) k adr)])
    -> Cofree (Diagram c d l) p
--     -> Either String ([p], AccuEmitState p [(Maybe p, ExtendedInstruction l (AI Int k adr))])
    -> m ([p], AccuEmitState p [(Maybe p, ExtendedInstruction l (AI (Reg adr) k adr))])
blargh accEmitDevice ast@(q0 :< _)
    = runStateT
        (traverseDiagram (accuEmit accEmitDevice) accuPost q0 ast)
        (AccuEmitState q0 sinks nodes [(q0, (0, Forever))] [])
    where
    (nodes, sinks) = collectNodesAndSinks ast --do i need this?

    --(d -> [ExtendedInstruction l (AI Int k adr)])
--     accEmitDevice = undefined


blarghX :: (Ord p, Show p, Show l, Monad m)
    => (d -> m [ExtendedInstruction l (AI (Reg adr) k adr)])
    -> [ (a, Cofree (Diagram c d l) p)]
--     -> Either String [(Maybe p, ExtendedInstruction l (AI Int k adr))]
    -> m [(Maybe p, ExtendedInstruction l (AI (Reg adr) k adr))]
blarghX emitDev  s = do
    chunks <- for s \(_lbl, ast) -> do
        blargh emitDev ast
    return (foldMap (aesCode.snd) chunks) --fixme 

--------------------------------------------------------------------------------

generateStk2THISISTOOMUCH
    :: (Show addr, Show word, Show lbl, Eq lbl, MonadError String m, Monad m)
    => (dev -> Either String x) --TODO swap (Either String) for m
    -> (x -> [Instruction word addr])
    -> [(Maybe lbl, Cofree (Diagram Void dev lbl) DgExt)]
    -> m [ExtendedInstruction Int (Instruction word addr)]
generateStk2THISISTOOMUCH doOp emitDev ast = do
    ast'  <- for ast (traverse (mapOpsM (liftEither . doOp))) --FIXME remove liftEither
    code  <- for ast' (traverse (generateStkOMFG emitDev))
    code' <- liftEither $ resolveLabels code
    return $ code' ++ [EIReturn]

generateStkOMFG
    :: (Show lbl, Eq lbl, Show addr, Show word, Monad m)
    => (device -> [Instruction word addr])
    -> Cofree (Diagram Void device lbl) DgExt
    -> m [ExtendedInstruction lbl (Instruction word addr)]
generateStkOMFG doDevice ast@(p0 :< _) = do
    let (nodes, sinks) = collectNodesAndSinks ast
    let Right (_, u) = runStateT
                    (traverseDiagram (stackEmit (pure . fmap EISimple . doDevice))
                        (\_ _ -> pure ()) p0 ast)
                    (StackEmitState [] sinks nodes [])
    return $ esCode u

compileForTest04
    :: (Show lbl, Eq lbl, MonadError String m, Monad m)
    => [( Maybe lbl
        , Cofree
            (Diagram Void (([(CellType, Operand Text)], DeviceImpl (V String) String)) lbl)
            DgExt
        )]
    -> m [ExtendedInstruction Int (Instruction (V String) String)]
compileForTest04 ast = generateStk2THISISTOOMUCH pure emitDevice03 ast
    where
    emitDevice03
        :: ([(CellType, Operand Text)], DeviceImpl (V String) String)
        -> [Instruction (V String) String]
    emitDevice03 = snd . emitDevice03'
        where
        emitDevice03'
            :: ([(CellType, Operand Text)], DeviceImpl (V String) String)
            -> ([String], [Instruction (V String) String])
        emitDevice03' (ops, impl) = case impl (fmap unAddr ops) of
                                    Left err -> error $ show (here, err)
                                    Right x -> x
            where
            unAddr :: (CellType, Operand Text) -> Operand String
            unAddr (_, Var a) = Var $ unpack a
            unAddr (_, Lit i) = Lit i

--------------------------------------------------------------------------------

pipeline :: Show e2 => (t -> ExceptT e IO t1)
                      -> (t1 -> ExceptT e1 IO [(a, t2)])
                      -> (t2 -> ExceptT e2 IO b)
                      -> ([(a, b)] -> ExceptT e3 IO b1)
                      -> t
                      -> IO b1
pipeline lexer postLex parser compile sourceText = do
    Right lxs <- runExceptT (lexer sourceText)
    Right lxs' <- runExceptT (postLex lxs)
    ast0 <- runExceptT $ for lxs' \(lbl, ast0) -> do
        x <- parser ast0
        return (lbl, x)
    ast <- case ast0 of
         Left err -> error $ show (here, err)
         Right ast1 -> return ast1
    Right code <- runExceptT $ compile ast
    return code

niceSrc :: (String -> IO ()) -> String -> Text -> IO ()
niceSrc outputLine file src = do
    outputLine $ "     ┊ " ++ file
--     outputLine $ "═════╪" ++ replicate 80 '═'
    outputLine $ "═════╪" ++ concat (replicate 7 "════╤════╦")
    for_ (zip [1::Int ..] (T.lines src)) \(i, ln) ->
        outputLine (printf "%4i ┊%s" i ln)
--     outputLine $ "═════╧" ++ replicate 80 '═'
    outputLine $ "═════╧" ++ concat (replicate 7 "════╧════╩")
-- outputLine  $ setSGRCode [SetItalicized True] ++ "hello"
-- outputLine  $ setSGRCode [Reset] ++ "hello"

main :: IO ()
main = do
    [file] <- getArgs
    src <- TIO.readFile file

    print (here, "--------------------------------------------------"::String)
--     TIO.putStrLn src
    niceSrc putStrLn file src
    print (here, "--------------------------------------------------"::String)
#if 1
    prog <- pipeline
        (liftEither . runLexer)
        (pure . labeledRungs . dropWhitespace)
        (liftEither . runLadderParser_ (wrapDevice3 (pure . I) (pure . A)) ladderLiberal)
        (liftEither . compileForTest04)
        src
    putStrLn "---------------------------"
    for_ prog print
    putStrLn "---------------------------"
#endif
    case runLexer src of
        Left err -> putStrLn err
        Right lxs -> do

            let lxs' = dropWhitespace lxs
            let blocks = labeledRungs lxs'

            forM_ blocks $ \(lbl, lxs'') -> do
                print (here, lbl)
                let zp = mkDgZp lxs''
                for_ (toList zp) (print . (here,))


--                 test2 lxs''


--                 case runLadderParser deviceThing ladderLiberal lxs'' of
--                     Left err -> print (here, err)
--                     Right (ast1, zp1) -> do
--                         print (here, "--------------------------------------------------")
--                         for_ (toList zp1) (print . (here,))
--                         print (here, "--------------------------------------------------")
--                         putStrLn ""
--                         print (here, toList ast1)
--                         print (here, nub $ toList ast1)
--                         test1 ast1
--                         return ()
#if 1
    prog1 <- pipeline
        (liftEither . runLexer)
        (pure . labeledRungs . dropWhitespace)
        (liftEither . runLadderParser_ deviceThing ladderLiberal)
        ( liftEither . blarghX accEmitDev1)
        src
    putStrLn "---------------------------"
    for_ prog1 (print.snd)
    putStrLn "---------------------------"
#endif
    return ()

    where
--     deviceThing = wrapDevice3 (pure . I) (pure . A)
    deviceThing = wrapDeviceSimple2

accEmitDev1
    :: (DevType Text, [Operand Text])
    -> Either String [ExtendedInstruction Text (AI (Reg Text) (V Text) Text)]
accEmitDev1 = f
    where
    simple = Right . map EISimple

    f (Contact_ " ", [Var a]) = simple [AIAnd $ M a]
    f (Contact_ "/", [Var a]) = simple [AINot, AIAnd $ M a]
    f (Contact_ ">", [a, b]) = simple $ getArg a ++ getArg b ++ [AIGt]
    f (Contact_ "<", [a, b]) = simple $ getArg a ++ getArg b ++ [AILt]
    f (Coil_    " ", [Var a]) = simple [AIStBit a]
    f (Coil_    "/", [Var a]) = simple [AINot, AIStBit a, AINot]
    f (Coil_    "S", [Var a]) = simple [AIOr $ M a, AIStBit a]
    f (Coil_    "R", [Var a]) = simple
        [AIStReg (R 0), AINot, AIAnd $ M a, AIStBit a, AILdReg (R 0)]
    f (         d  , args) = error $ show (d, args)

    getArg (Var a) = [ AILdCnA (A a), AILdM ]
    getArg (Lit i) = [ AILdCnA (I i) ]
