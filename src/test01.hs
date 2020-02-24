{-# OPTIONS_GHC -Wunused-imports  -Wall #-}
{-# LANGUAGE CPP, TupleSections, FlexibleContexts, ScopedTypeVariables
    , BlockArguments
    , OverloadedStrings
#-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Foldable
import Data.Void

-- import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Simple
import Language.Ladder.Interpreter
import Language.Ladder.Types
import Language.Ladder.Utils

import Control.Monad.State
-- import Control.Monad.Writer
-- import Control.Monad.Error
import Control.Monad.Except
-- import qualified Data.Map as M
import Data.Bifunctor
import Data.List
-- import Data.Function
-- import Data.Proxy
import Data.Text (Text, unpack)
import qualified Data.Text as T --(Text, lines)
import Text.Printf
-- import Data.Functor.Identity

import Data.Traversable

import System.Console.ANSI.Types
import System.Console.ANSI.Codes

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

--------------------------------------------------------------------------------

type AccItpSt a = (Bool, [Bool], [V a], Memory a)

accEval :: (Eq address, Show address)
     => AI Int (V address) address
     -> AccItpSt address
     -> Either
         (AccItpSt address, String)
         (AccItpSt address)
accEval = go
    where
    go  AITrap     st                = halt st "trap"
    go  AILdOn        (a, rf, os, m) = pure (True, rf, os, m)

    go (AILdReg r) st@(_, rf, os, m) = (,rf,os,m) <$> getReg r st

    go (AIStReg r) st@(a, rf, os, m)
        | (f,v:g) <- splitAt r rf    = pure (a, f ++ a : g, os, m)
        | otherwise                  = halt st (show ("rf idx out of range"::String, r, rf))
    go (AILdBit i) st@(_, rf, os, m)
        | Just (X v) <- lookup i m   = pure (v, rf, os, m)
        | otherwise                  = halt st (show (here, "invalid memory access"::String, i))
    go (AIStBit i) st@(a, rf, os, m)
        | (m0,(_,X _):m1) <- break ((i==) . fst) m
                                     = pure (a, rf, os, (m0 ++ (i, X a) : m1))
        | otherwise                  = halt st (show (here, "invalid memory access"::String, i))

--     go (AIAnd   r) st@(a, rf, os, m) = getReg r st >>= \b -> pure (a && b, rf, os, m)
    go (AIAnd   r) st@(a, rf, os, m) = (,rf,os,m) <$> ((a &&) <$> getReg r st)

    go (AIOr    r) st@(a, rf, os, m) = (,rf,os,m) <$> ((a ||) <$> getReg r st)
    go  AINot      st@(a, rf, os, m) = pure (not a, rf, os, m)

    go (AILdCnA k) (a, rf, os, m)    = pure (a, rf, k : os, m)
    go  AILdM      st@(a, rf, A i : os, m)
       | Just v <- lookup i m        = pure (a, rf, v : os, m)
       | otherwise                   = halt st (show (here, "invalid memory access"::String, i))
    go  AIStM      st = undefined --TODO
    go  AIEq       st = undefined
    go  AILt       (_, rf,   I b : I a : os, m) = pure (a < b, rf, os, m)
    go  AIGt       (_, rf,   I b : I a : os, m) = pure (a > b, rf, os, m)
    go  i          st                = halt st (show i)

    halt st e = Left (st, show e)

    getReg r st@(a, rf, os, m)
--         | r >= 0 && r < length rf = pure (rf !! r, rf, os, m)
        | r >= 0 && r < length rf = pure (rf !! r)
        | otherwise               = Left (st, show ("rf idx out of range"::String, r, rf))

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
    go  AITrap     st                = halt st "trap"
    go  AILdOn        (a, os, m) = pure (True, os, m)

    go (AILdReg r) st@(_, os, m) = (,os,m) <$> getReg r st

--     go (AIStReg r) st@(a, os, m)
--         | (f,v:g) <- splitAt r rf    = pure (a, f ++ a : g, os, m)
--         | otherwise                  = halt st (show ("rf idx out of range"::String, r, rf))
    go (AIStReg r) st = storeBit r st
--     go (AILdBit i) st@(_, os, m)
--         | Just (X v) <- lookup i m   = pure (v, os, m)
--         | otherwise                  = halt st (show (here, "invalid memory access"::String, i))
    go (AILdBit i) st@(a, os, m) = (,os,m) <$> loadBit i st
--     go (AIStBit i) st@(a, os, m)
--         | (m0,(_,X _):m1) <- break ((i==) . fst) m
--                                      = pure (a, os, (m0 ++ (i, X a) : m1))
--         | otherwise                  = halt st (show (here, "invalid memory access"::String, i))
    go (AIStBit i) st = storeBit i st
    go (AIAnd   r) st@(a, os, m) = (,os,m) <$> ((a &&) <$> getReg r st)
    go (AIOr    r) st@(a, os, m) = (,os,m) <$> ((a ||) <$> getReg r st)
    go  AINot      st@(a, os, m) = pure (not a, os, m)

    go (AILdCnA k) (a, os, m)    = pure (a, k : os, m)
    go  AILdM      st@(a, A i : os, m)
       | Just v <- lookup i m        = pure (a, v : os, m)
       | otherwise                   = halt st (show (here, "invalid memory access"::String, i))
    go  AIStM      st = undefined --TODO
    go  AIEq       st = undefined
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

    go (Node   []      ) = do
        accSet p -- "optimization"
    go (Node   w       ) = do
        sinks <- gets aesSinks
        let deps = (foldMap (checkDataDep sinks) w) ++ [] -- ugh
        getValue q
        for_ deps \d -> do
            r <- getFromRegs d
--             accEmit' [ "or $" ++ show r ]
            accEmit' [ EISimple $ AIOr $ R r ]
        accSet p
--         when (length w > 1) do accSpill (length w) p
        unless (null w) do accSpill (length w) p
    go  Sink             = do
        nodes <- gets aesNodes
        if elem p nodes
        then do
            accSet p
            accSpill 1 p
        else return ()
    go (Source _a       ) = do
--         accEmit' [ "ld #1" ]
        accEmit' [ EISimple AILdOn ]
        accSet p
    go  End              = do
        return ()
    go (Device device _a) = do
        getValue q
--         accEmit' [ "eval " ++ show device ++ " // " ++ show p ]
        lift (emitDevice device) >>= accEmit'
        accSet p
    go (Jump   label  ) = do
        getValue q
--         accEmit' [ "cjmp " ++ show label ]
        accEmit' [ EIJump label ]
    go (Cont   _continuation _a) = undefined
    go (Conn   _continuation) = undefined

    --find value in registers
    getValue pp = do
        accu <- gets aesAccu
        if accu == pp
        then return ()
        else getFromRegs pp >>= load

    accEmit' = accEmit . fmap (Just p,)

--     load r = accEmit' [ "ld $" ++ show r ]
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
--                 let rf' = rf --TODO
                modify \st -> st { aesRegisters = rf'}
                return v

-- accEmit ::  MonadState (AccuEmitState p [a]) m => [a] -> m ()
accEmit s = modify \st -> st { aesCode = aesCode st ++ s }
-- accSet :: MonadState (AccuEmitState p w) m => p -> m ()
accSet q = modify \st -> st { aesAccu = q }

-- accSpill :: Int -> p -> StateT
--         (AccuEmitState p [String])
--         (Either String) ()
accSpill uses name = do
    rf   <- gets aesRegisters

    let rUseNumber = snd . snd
    case break ((UseCount 0==) . rUseNumber) rf of
        (_pre, []) -> do --free register not found
            modify \st -> st { aesRegisters =  rf ++ [(name, (length rf, UseCount uses))] }
--             accEmit [ (Nothing, "st $" ++ show (length rf)) ] 
            accEmit [ (Nothing, EISimple $ AIStReg (R $ length rf)) ] 
        (pre, (_ppp, (v, _uses)) : rest) -> do
            modify \st -> st { aesRegisters =  pre ++ (name, (v, UseCount uses)) : rest }
--             accEmit [ (Nothing, "st $" ++ show v) ] 
            accEmit [ (Nothing, EISimple $ AIStReg $ R v) ] 

--     modify \st -> st { aesRegisters =  rf ++ [(name, (length rf, uses))] }
--     accEmit [ "st $" ++ show (length rf) ] 

-- accuPost
--     :: a
--     -> Cofree (Diagram c d l) a
--     -> StateT (AccuEmitState a [String]) (Either String) ()
accuPost :: (Monad m)
    => p
    -> Cofree (Diagram continuation device label) a
    -> StateT
        (AccuEmitState p [(Maybe p, ExtendedInstruction l (AI (Reg adr) k adr))])
        m -- (Either String)
        ()

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
        (AccuEmitState q0 sinks nodes [] [])
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

--TODO TODO TODO

data Setup src t lxs ast d code m = Setup
    { sLexer        :: t -> m lxs
    , sDeviceParser :: t -> m d
    , sParser       :: (t -> m d) -> lxs -> m ast
    , sDeviceEmit   :: d -> m code
    , sCodeGen      :: ast -> m code
    }

-- accSetup = Setup
--     { sLexer        = undefined
--     , sDeviceParser = undefined
--     , sParser       = undefined
--     , sDeviceEmit   = undefined
--     , sCodeGen      = undefined
--     }

{-
data Failure = Lexing | Parsing | CodeGen | ...
:: Setup -> m Failure
-}

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
            (Diagram
                Void
                (([(CellType, Operand Text)], DeviceImpl (V String) String))
                lbl
            )
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

-- -- ehlo
-- --     :: [(Maybe String
-- --             , Cofree (Diagram Void 
-- --                     (([(CellType, Operand Text)], DeviceImpl (V String) String))
-- --                     String) DgExt)]
-- --     -> IO ()
-- ehlo ast = do
-- --     when verbose $ print here
--     prog <- either fail pure $ compileForTest03 ast
-- --     let memSlots :: [(CellType, Text)] = nub $ execState (traverse_ (traverse_ mp2) ast) []
-- --     print (here, memSlots, "<<<<<"::String)
-- --     print (here, memSlotsToTestVector 4 memSlots, "<<<<<"::String)
-- 
--     putStrLn "---------------------------"
--     for_ prog print
--     putStrLn "---------------------------"
-- 
-- --     where
-- --     mp2 :: Cofree
-- --                         (Diagram c' ([(CellType, Operand txt)], b) s') a
-- --                       -> StateT [(CellType, txt)] Identity ()
-- --     mp2 (_ :< n) = do
-- --         void $ mapDgA pure ((modify.(++)).addressesOnly.fst) pure n
-- --         traverse_ mp2 n
-- -- 
-- --     addressesOnly s = [(t, a)|((t, Var a)) <- s]

--------------------------------------------------------------------------------

-- test1 ast1 = do
--     let Right (_, st) = blargh ast1
--     print (here)
--     for_ (aesCode st) print
--     return ()

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

-- test2 lxs = do
-- 
--     case runLadderParser_ (wrapDevice3 (pure . I) (pure . A)) ladderLiberal lxs of
--         Left  err -> print (here, err)
--         Right ast -> do
--             print here
--             ehlo [(Nothing, ast)]
-- --             for_ ((esCode u) :: [ExtendedInstruction T.Text Int Int]) print
-- --             for_ (esCode u) print
-- --             print $ length $ esStack u
--             return ()

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

    print (here, "--------------------------------------------------")
--     TIO.putStrLn src
    niceSrc putStrLn file src
    print (here, "--------------------------------------------------")
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
    ;
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
    deviceThing = wrapDeviceSimple

--     accEmitDev2 :: ([(CellType, Operand Text)], DeviceImpl (V String) String)
--              -> Either String [ExtendedInstruction Text (AI Int Int Int)]
--     accEmitDev2 (x, y) = (error . show) x

--     accEmitDev1 :: d -> Either String [ExtendedInstruction Text (AI Int Int Int)]
    accEmitDev1 :: (DevType Text, [Operand Text])
                      -> Either String [ExtendedInstruction Text (AI (Reg Text) Int Text)]
    accEmitDev1 (Contact_ " ", [Var a]) = pure [EISimple $ AIAnd $ M a]
    accEmitDev1 (Contact_ "/", _args) = pure undefined
    accEmitDev1 (Contact_ ">", _args) = pure undefined
    accEmitDev1 (Contact_ "<", _args) = pure undefined
    accEmitDev1 (Contact_ d, _args) = error $ show d
    accEmitDev1 (Coil_ " ", [Var a]) = pure [EISimple $ AIStBit a]
    accEmitDev1 (Coil_ "/", _args) = pure undefined
    accEmitDev1 (Coil_ "S", _args) = pure undefined
    accEmitDev1 (Coil_ "R", _args) = undefined
    accEmitDev1 (Coil_ d, _args) = pure undefined
