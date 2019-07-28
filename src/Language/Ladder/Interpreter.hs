{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleInstances #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Interpreter where

import Data.Foldable
import Data.Traversable
import Control.Monad.Writer.Strict
import Data.List
import Data.String
import Data.Void

import qualified Control.Monad.Fail --FIXME

import Language.Ladder.DiagramParser (DgExt)
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Analysis

--------------------------------------------------------------------------------

data CellType = Bit | TwoBits | Word -- | TON | TOF
    deriving (Show, Read, Eq, Ord)

-- |Memory cell value, also represents its type and default value
data V
    = X !Bool
    | I !Int
    | A !String --FIXME parametrize
    deriving (Show, Read, Eq)

-- data V2 = T | F | I Int
--     deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

data ExtendedInstruction label word address
    = EIJump   !label
    | EISimple !(Instruction word address)
    | EIReturn
    deriving (Show, Eq)

data Instruction w a
    = ITrap --invoke debugger
--     | ISysRq
    | ILdOn -- push #1 onto wire stack {- w: -- #1 -}
    | IDup -- coudl be replaced by IPick 0, dup value on top of wire stack {- w: x -- x x -}
    | IPick  !Int -- push wire stack value at index onto wire stack {- w: -- x -}
    | IDrop --drop value from wire stack {- w: x -- -}

    | ILdBit !a -- push bit from address onto wire stack
    | IStBit !a -- dtto for store

    | IAnd -- and two values on wire stack, push result back
    | IOr -- dtto, or
    | INot -- negate value on top of wire stack
--     | IXor

    | ILdCnA !w {- push int const onto argument stack, a: -- l -}
    | ILdM {- a: size addr -- <value> -}
    | IStM

--     | IOp Operator --instead of lt,gt,etc
    | IEq -- compare two value on arg stack and push result onto wire stack
    | ILt
    | IGt
--     | IMov a a
    deriving (Show, Eq)

--------------------------------------------------------------------------------

--get rid of text
resolveLabels
    :: (Show lbl, Eq lbl)
    => [(Maybe lbl, [ExtendedInstruction lbl w a])]
    -> Either String [ExtendedInstruction Int w a]
resolveLabels l = for (foldMap snd l) g
    where
    l' = fmap (fmap length) l

    (_, l'') = foldl f (0, []) l'
    f (acc, xs) (lbl, blkLen) = (acc + blkLen, (lbl, acc) : xs)

    g (EIJump lbl) = case lookup (Just lbl) l'' of
                       Just a  -> Right (EIJump a)
                       Nothing -> Left $ show (here, lbl)
    g (EISimple i) = Right (EISimple i)
    g  EIReturn    = Right EIReturn

--------------------------------------------------------------------------------

-- could hide writer monad stuff
-- or data I m  = I { ldOn :: m () }
--not that i need monad, monoid or even semigroup would do the job

{-
class I d m where
    emLdOn :: m ()
    emDrop :: m ()
    emDev :: d -> m ()
--     emBranch :: lbl -> m () -- ???
    emPick :: Int -> m ()
    emDup :: m ()
    -}

-- data Emit ca w d m = Emit
--     { emLdOn :: m ()
--     , emDrop :: m ()
--     , emDevice :: d -> m ()
--     , emBranch :: ca -> m () -- ???
--     , emPick :: Int -> m ()
--     , emDup :: m ()
--     }

--------------------------------------------------------------------------------

genStk :: (
--     Show a0    , 
        Show label0, Monad m0)
     => ([ExtendedInstruction label0 word address] -> m0 ())
                      -> (a0 -> [Instruction word address])
                      -> [Cofree (Diagram DgExt a0 label0) DgExt]
                      -> Cofree (Diagram DgExt a0 label0) DgExt
                      -> m0 [Cofree (Diagram DgExt a0 label0) DgExt]
genStk emit' emitDevice' stk0 asts = go stk0 asts

    where
    emit = emit' . fmap EISimple
    emitDevice = emit . emitDevice'

    go stack nd@(p :< a) = f stack a
        where

        f stk     (Source b)      = do
            emit [ILdOn]
            go (nd:stk) b
        f (_:stk)  Sink           = do
            return (nd:stk)
--             case lookup p sinkToNode of --XXX here is argument for distinguishing 'Sink' and 'Stub'
--                 Just _  -> return (nd:stk) --put value back under name with which is referenced
--                 Nothing -> do
--                     emit [IDrop]
--                     return (stk)
        f stk      End         = return (stk) --XXX emit Drop?
        f (x:stk) (Device d b)    = do
            emitDevice d
            go (nd:stk) b
        f (_:stk) (Jump s)        = do
            emit' [EIJump s]
            return stk
        f (_:stk) (Node b)        = do
--i think dups should be emitted only AFTER node value is computed
    --fold over stack, picking sinks(aka stubs), emitting Or's
            --depth of stack stays the same during evaluation of preds
            for_ (zip stk [0..]) $ \(x, i) -> do
                case x of
                     pp :< Sink | pp == p -> do
                         bringToTop i
                         emit [IOr]
                     _ -> return ()

            let copiesOnStack = fmap (const nd) b
            replicateM (length b - 1) $ emit [IDup]

            foldlM
                (go) --emit Dup's here? --so that stack level is kept low
                (copiesOnStack ++ stk)
                b
        f (pp:stk) (Conn c)       = do
            return (nd:stk)
        f stk      (Cont stubP b) = do
            case findIndex (isConn stubP) stk of
                Just i  -> bringToTop i
                Nothing -> error here --should not happen
            go (nd:stk) b --XXX not sure about nd on stack here

        f stk n = error here -- $ show (here, stk, n)

    isConn p0 (_ :< Conn p1) = p0 == p1
    isConn _   _             = False

    bringToTop 0 = return ()
    bringToTop i = emit [IPick i]

--------------------------------------------------------------------------------

verbose1 = False

generateStk2'
    :: (Show lbl, Eq lbl
    , Show addr
    , Show word
--     , Show device
    )
    => (Int -> IO word)
    -> (device -> [Instruction word addr])
    -> Cofree (Diagram Void device lbl) DgExt
    -> IO [ExtendedInstruction lbl word addr]
generateStk2' literalFromInt doDevice ast' = do
    let ast = dropEnd ast'
    --collapse nodes
    let (nodes, a0) = repositionSinks nodes <$> merge' ast
    --chop
    let Just a1 = forest a0
    let a5 = cut1' a1
    let a6 = sortOn position a5
    let a7 = tsort3 a6

    code <- execWriterT $ foldlM (genStk tell doDevice) [] a7 --need failure here
--     when verbose1 $ do
--         print (here, "-----------------------")
--         for_ a1 print
--         print (here, "nodes", "-----------------------")
--         for_ nodes print
--         print (here, "after sort on position", "-----------------------")
--         for_ a6 print
--         print (here, "after tsort2", "-----------------------")
--         for_ a7 print
--         print (here, "-----------------------")
--         for_ code print
--         print (here, "-----------------------")
    return code

--------------------------------------------------------------------------------

type Program a = [ExtendedInstruction Int V a]

-- data Trigger = Periodic Int | Memory String
data Task a = Task { nextRun, priority, period :: Int, program :: Program a }

type ItpSt3 a = (Clock, [Task a], Memory a)
type Period = Int
type Clock = Int
type Prio = Int

makeItpSt3 :: Memory a -> [(Period, Prio, Program a)] -> ItpSt3 a
makeItpSt3 m tasks = (0, fmap (\(per, pri, pro) -> Task 0 pri per pro) tasks, m)

--FIXME error should return also offending task
run :: (Eq address, Show address)
    => ItpSt3 address
    -> Either (ItpSt address, String) (ItpSt3 address)
run (clk, tasks, st0)
    = (clk + 1, waiting ++ nextRound,)
    <$> foldlM execute st0 run''
    where
    (runnableNow, waiting) = partition ((clk==).nextRun) tasks

--FIXME use proper names
    run'      = sortOn priority runnableNow
    run''     = fmap program run'
    nextRound = fmap (\tsk@Task{..} -> tsk { nextRun = clk + period }) run'

--------------------------------------------------------------------------------

execute :: (Eq address, Show address)
         => Memory address
        -> [ExtendedInstruction Int V address]
        -> Either (ItpSt address, String) (Memory address)
execute mem0 prog = (\(_, _, m) -> m) <$> f prog ([], [], mem0)
    where

    f []               st = return st
    f (EIReturn   : _) st = return st
    f (EIJump lbl : p) st@(w:ws, os, m)
        | w               = nextLabel lbl ([], [], m) --XXX beware! jump clears stacks!
        | otherwise       = f p (ws, os, m)
    f (EIJump _ : _) _    = error here --stack underflow FIXME proper fail
    f (EISimple i : p) st = eval st i >>= f p

    nextLabel lbl = f (drop lbl prog)

--------------------------------------------------------------------------------

type Memory a = [(a, V)]

type ItpSt a = ([Bool], [V], Memory a)

eval :: (Eq address, Show address)
     => ItpSt address
     -> Instruction V address
     -> Either
         (ItpSt address, String)
         (ItpSt address)
eval = f
    where
    f st                 ITrap      = Left (st, "trap")
    f    (  ws, os, m)   ILdOn      = pure (True : ws, os, m)
    f    (w:ws, os, m)   IDup       = pure (w : w : ws, os, m)
    f st@(  ws, os, m)  (IPick i)
        | i >= 0 && i < length ws   = pure (ws !! i : ws, os, m)
        | otherwise                 = Left (st, "stk idx out of range")
    f    (_:ws, os, m)   IDrop      = pure (ws, os, m)
    f st@(ws,   os, m)  (ILdBit a)
        | Just (X v) <- lookup a m  = pure (v : ws, os, m)
        | otherwise                 = Left (st, "invalid memory access")
    f st@(w:ws, os, m)  (IStBit a)
        | (m0,(_,X _):m1) <- break ((==a) . fst) m
                                    = pure (w : ws, os, (m0 ++ (a, X w) : m1))
        | otherwise                 = Left (st, "invalid memory access")
    f st@(a:b:ws, os, m) IAnd       = pure ((a && b) : ws, os, m)
    f st@(a:b:ws, os, m) IOr        = pure ((a || b) : ws, os, m)
    f st@(a:ws,   os, m) INot       = pure (not a : ws,  os, m)

    f st@(ws,   os, m) (ILdCnA k) = pure (ws,  k : os, m)

--     f st@(ws,   A a : os, m)  ILdM
--         | Just v <- lookup a m  = pure (ws, v : os, m)
--         | otherwise                 = Left (st, "invalid memory access")

    f _                  i          = error $ show (here, i)

--     f    (ItSt ws     os         m) (ILdArg o) = pure $ ItSt ws (o:os) m
--     f st@(ItSt ws     (Var n:os) m)  ILdM
--         | Just v <- lookup n m                 = undefined --pure $ ItSt ws os m
--         | otherwise                            = Left (st, "var not found")

--------------------------------------------------------------------------------

data RW = Rd | Wr
    deriving (Show)

data DeviceDescription n impl = DDesc !n ![(RW, CellType)] !impl

type DeviceImpl word addr = [Operand addr] -> Either String [Instruction word addr]

type Devices word addr name =
--    (Integral word, Integral addr, IsString name) => 
    [(DevType name,
            DeviceDescription name (DeviceImpl word addr)
            )]

--OUCH!!!
instance Control.Monad.Fail.MonadFail (Either String) where
    fail = Left

--backend for this interpreter
-- devices
--     :: (Integral word, Integral addr, IsString name)
--     => [(DevType name,
--             DeviceDescription
--                 name
--                 ([Operand addr] -> Either a [Instruction word addr]))]
devices
    :: IsString name
    => (Int -> Either String word)
    -> (addr -> Either String word)
    -> Devices word addr name
devices mkWord litFromAddr =
    [ (Contact_ " ", DDesc "AND"  [(Rd, Bit)] (\[Var a] -> Right [ILdBit a, IAnd]))
    , (Contact_ "/", DDesc "ANDN" [(Rd, Bit)] (\[Var a] -> Right [ILdBit a, INot, IAnd]))
    , (Contact_ ">", DDesc "GT" [(Rd, Word), (Rd, Word)]
        (\[a, b] -> do
            a' <- emitOp a
            b' <- emitOp b
            Right $ a' ++ b' ++ [IGt]))
    , (Contact_ "<", DDesc "LT" [(Rd, Word), (Rd, Word)]
        (\ops -> do
            [a, b] <- for ops emitOp
            Right $ a ++ b ++ [ILt]))

    , (Coil_    " ", DDesc "ST"   [(Wr, Bit)] (\[Var a] -> Right [IStBit a]))
    , (Coil_    "/", DDesc "STN"  [(Wr, Bit)] (\[Var a] -> Right [INot, IStBit a, INot]))
    , (Coil_    "S", DDesc "SET"  [(Wr, Bit)]
        (\[Var a] -> Right [ILdBit a, IOr, IStBit a]))
    , (Coil_    "R", DDesc "RST"  [(Wr, Bit)]
        (\[Var a] -> Right [INot, ILdBit a, IAnd, IStBit a]))
    ]
    where
    emitOp a = case a of
                 Lit i -> mkWord i >>= \i' -> pure [ILdCnA i']
                 Var addr -> litFromAddr addr >>= \addr' -> Right [ILdCnA addr', ILdM]

-- w m m'
-- 0 0 0
-- 0 1 1
-- 1 0 0
-- 1 1 0

--------------------------------------------------------------------------------
