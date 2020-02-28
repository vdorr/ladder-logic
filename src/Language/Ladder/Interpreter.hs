#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Interpreter
--     ( CellType(..)
--     , Instruction(..)
--     , ExtendedInstruction(..)
--     , DeviceImpl
--     , DeviceDescription(..)
--     , resolveLabels
--     , generateStk2'
--     , devices
--     , Memory
--     , V(..)
--     , makeItpSt3
--     , run
--     , stackEmit
--     , StackEmitState(..)
--     )
    where

import Data.Foldable
import Data.Traversable
import Control.Monad.Writer.Strict
import Data.List
import Data.String
import Data.Void
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Identity

import Language.Ladder.Utils
import Language.Ladder.Types
import Language.Ladder.Eval (Memory, V(..), CellType(..))
import Language.Ladder.LadderParser (DeviceParser, DevOpFlag(..))

--------------------------------------------------------------------------------

data ExtendedInstruction label si
    = EIJump   !label
    | EISimple !si
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
    => [(Maybe lbl, [ExtendedInstruction lbl si])]
    -> Either String [ExtendedInstruction Int si]
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

-- genStk :: (
--         Show label0, Monad m0)
--      => ([ExtendedInstruction label0 (Instruction word address)] -> m0 ())
--                       -> (a0 -> [Instruction word address])
--                       -> [Cofree (Diagram DgExt a0 label0) DgExt]
--                       -> Cofree (Diagram DgExt a0 label0) DgExt
--                       -> m0 [Cofree (Diagram DgExt a0 label0) DgExt]
-- genStk emit' emitDevice' stk0 asts = go stk0 asts
-- 
--     where
--     emit = emit' . fmap EISimple
--     emitDevice = emit . emitDevice'
-- 
--     go stack nd@(p :< a) = f stack a
--         where
-- 
--         f stk     (Source b)      = do
--             emit [ILdOn]
--             go (nd:stk) b
--         f (_:stk)  Sink           = do
--             return (nd:stk)
--         f stk      End         = return (stk) --XXX emit Drop?
--         f (_:stk) (Device d b)    = do
--             emitDevice d
--             go (nd:stk) b
--         f (_:stk) (Jump s)        = do
--             emit' [EIJump s]
--             return stk
--         f (_:stk) (Node b)        = do
-- --i think dups should be emitted only AFTER node value is computed
--     --fold over stack, picking sinks(aka stubs), emitting Or's
--             --depth of stack stays the same during evaluation of preds
--             for_ (zip stk [0..]) $ \(x, i) -> do
--                 case x of
--                      pp :< Sink | pp == p -> do
--                          bringToTop i
--                          emit [IOr]
--                      _ -> return ()
-- 
--             let copiesOnStack = fmap (const nd) b
--             replicateM_ (length b - 1) $ emit [IDup]
-- 
--             foldlM
--                 (go) --emit Dup's here? --so that stack level is kept low
--                 (copiesOnStack ++ stk)
--                 b
--         f (_:stk) (Conn _)       = do
--             return (nd:stk)
--         f stk      (Cont stubP b) = do
--             case findIndex (isConn stubP) stk of
--                 Just i  -> bringToTop' (i) -- +100)
--                 Nothing -> error here --should not happen
--             go (nd:stk) b --XXX not sure about nd on stack here
-- 
--         f _stk _n = error here -- show (here, stk, n)
-- 
--     isConn p0 (_ :< Conn p1) = p0 == p1
--     isConn _   _             = False
-- 
-- --     bringToTop 0 = emit [IDup] --return ()
--     bringToTop 0 = return ()
--     bringToTop i = emit [IPick i]
-- 
-- --     bringToTop' 0 = emit [IDup] --return ()
--     bringToTop' i = emit [IPick i]
-- 
-- generateStk2'
--     :: (Show lbl, Eq lbl
--     , Show addr
--     , Show word
-- --     , Show device
--     , Monad m
--     )
--     => (device -> [Instruction word addr])
--     -> Cofree (Diagram Void device lbl) DgExt
--     -> m [ExtendedInstruction lbl (Instruction word addr)]
-- generateStk2' doDevice ast' = do
--     let ast = dropEnd ast'
--     --collapse nodes
--     let (nodes, a0) = repositionSinks nodes <$> merge' ast
--     --chop
--     let Just a1 = forest a0
--     let a5 = cut1' a1
--     let a6 = sortOn position a5
--     let a7 = tsort3 a6
-- 
--     execWriterT $ foldlM (genStk tell doDevice) [] a7

generateStk2'
    :: (Show lbl, Eq lbl
    , Show addr
    , Show word
--     , Show device
    , Monad m
    )
    => (device -> [Instruction word addr])
    -> Cofree (Diagram Void device lbl) DgExt
    -> m [ExtendedInstruction lbl (Instruction word addr)]

generateStk2' doDevice ast@(p0 :< _) = do
--     undefined
    let (nodes, sinks) = collectNodesAndSinks ast
    let Right (_, u) = runStateT
                    (traverseDiagram (stackEmit (pure . fmap EISimple . doDevice))
                        (\_ _ -> pure ()) p0 ast)
                    (StackEmitState [] sinks nodes [])
    return $ esCode u

--------------------------------------------------------------------------------

type ItpSt a = ([Bool], [V a], Memory a)

type Program a = [ExtendedInstruction Int (Instruction (V a) a)]

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
        -> [ExtendedInstruction Int (Instruction (V address) address)]
        -> Either (ItpSt address, String) (Memory address)
execute mem0 prog = (\(_, _, m) -> m) <$> f prog ([], [], mem0)
    where

    f []               st = return st
    f (EIReturn   : _) st = return st
    f (EIJump lbl : p) (w:ws, os, m)
        | w               = nextLabel lbl ([], [], m) --XXX beware! jump clears stacks!
        | otherwise       = f p (ws, os, m)
    f (EIJump _ : _) _    = error here --stack underflow FIXME proper fail
    f (EISimple i : p) st = eval st i >>= f p

    nextLabel lbl = f (drop lbl prog)

-- execute' :: (Eq address, Show address)
--          => (ItpSt address
--             -> si
--             -> Either
--                 (ItpSt address, String)
--                 (ItpSt address))
--         -> Memory address
--         -> [ExtendedInstruction Int si]
--         -> Either (ItpSt address, String) (Memory address)
-- execute' go mem0 prog = (\(_, _, m) -> m) <$> f prog ([], [], mem0)
--     where
-- 
--     f []               st = return st
--     f (EIReturn   : _) st = return st
--     f (EIJump lbl : p) (w:ws, os, m)
--         | w               = nextLabel lbl ([], [], m) --XXX beware! jump clears stacks!
--         | otherwise       = f p (ws, os, m)
--     f (EIJump _ : _) _    = error here --stack underflow FIXME proper fail
--     f (EISimple i : p) st = go st i >>= f p
-- 
--     nextLabel lbl = f (drop lbl prog)

--------------------------------------------------------------------------------

eval :: (Eq address, Show address)
     => ItpSt address
     -> Instruction (V address) address
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
        | otherwise
            = Left (st, show ("stk idx out of range"::String, i, ws))
    f    (_:ws, os, m)   IDrop      = pure (ws, os, m)
    f st@(ws,   os, m)  (ILdBit a)
        | Just (X v) <- lookup a m  = pure (v : ws, os, m)
        | otherwise                 = Left (st, show (here, "invalid memory access"::String, a))
    f st@(w:ws, os, m)  (IStBit a)
        | (m0,(_,X _):m1) <- break ((==a) . fst) m
                                    = pure (ws, os, (m0 ++ (a, X w) : m1))
        | otherwise                 = Left (st, show (here, "invalid memory access"::String, a))
    f (a:b:ws, os, m)    IAnd       = pure ((a && b) : ws, os, m)
    f (a:b:ws, os, m)    IOr        = pure ((a || b) : ws, os, m)
    f (a:ws,   os, m)    INot       = pure (not a : ws,  os, m)

    f (ws,   os, m)     (ILdCnA k)  = pure (ws,  k : os, m)

    f _ IStM = undefined --TODO

    f st@(ws,   A a : os, m) ILdM
        | Just v <- lookup a m  = pure (ws, v : os, m)
        | otherwise                 = Left (st, show (here, "invalid memory access"::String, a))

    f (ws,   I b : I a : os, m)  IGt = pure ((a > b) : ws,  os, m)
    f (ws,   I b : I a : os, m)  ILt = pure ((a < b) : ws,  os, m)

    f _                  i          = error $ show (here, i)

--     f    (ItSt ws     os         m) (ILdArg o) = pure $ ItSt ws (o:os) m
--     f st@(ItSt ws     (Var n:os) m)  ILdM
--         | Just v <- lookup n m                 = undefined --pure $ ItSt ws os m
--         | otherwise                            = Left (st, "var not found")

--------------------------------------------------------------------------------

data RW = Rd | Wr
    deriving (Show)

data DeviceDescription n impl = DDesc !String ![(RW, CellType)] !impl

type DeviceImpl word addr = [Operand addr] -> Either String ([addr], [Instruction word addr])

type Devices word addr name =
    [(DevType name, DeviceDescription name (DeviceImpl word addr))]

devices
    :: IsString name
    => (Int -> Either String word)
    -> (addr -> Either String word)
    -> Devices word addr name
devices mkWord litFromAddr =
    [ (cont " ", DDesc "AND"  [(Rd, Bit)] (\[Var a] -> Right ([], [ILdBit a, IAnd])))
    , (cont "/", DDesc "ANDN" [(Rd, Bit)] (\[Var a] -> Right ([], [ILdBit a, INot, IAnd])))
    , (cont ">", DDesc "GT" [(Rd, Word), (Rd, Word)]
        (\[a, b] -> do
            a' <- emitOp a
            b' <- emitOp b
            Right ([], a' ++ b' ++ [IGt])))
    , (cont "<", DDesc "LT" [(Rd, Word), (Rd, Word)]
        (\ops -> do
            ops' <- for ops emitOp
            (a, b) <- case ops' of
                 [a, b] -> return (a, b)
                 _ -> Left here
            Right ([], a ++ b ++ [ILt])))

    , (coil    " ", DDesc "ST"   [(Wr, Bit)] (\[Var a] -> Right ([], [IDup, IStBit a])))
    , (coil    "/", DDesc "STN"  [(Wr, Bit)] (\[Var a] -> Right ([], [IDup, INot, IStBit a])))

    , (coil    "S", DDesc "SET"  [(Wr, Bit)]
        (\[Var a] -> Right ([], [IDup, ILdBit a, IOr, IStBit a])))
    , (coil    "R", DDesc "RST"  [(Wr, Bit)]
        (\[Var a] -> Right ([], [IDup, INot, ILdBit a, IAnd, IStBit a])))
    ]
    where
    cont = Contact_ . fromString 
    coil = Coil_ . fromString 
    emitOp a = case a of
                 Lit i -> mkWord i >>= \i' -> pure [ILdCnA i']
                 Var addr -> litFromAddr addr >>= \addr' -> Right [ILdCnA addr', ILdM]

--------------------------------------------------------------------------------

stackEmit
    :: (Eq p, Monad m)
    => (d -> m [ExtendedInstruction l (Instruction k adr)])
    -> p
    -> p
    -> Diagram c d l (Cofree (Diagram c d l) p)
    -> StateT
        (StackEmitState p [ExtendedInstruction l (Instruction k adr)])
        m --(Either String)
        p --TODO use MonadState constraint
stackEmit emitDevice q p x = go x *> pure p
    where

    go (Node   []      ) = do
--         pop
--         liftIO $ print (">>>>> DROP")
        return () -- "optimization"
    go (Node   w       ) = do
        sinks <- gets esSinks
        let deps = (foldMap (checkDataDep sinks) w) ++ [] -- ugh
        bringToTop q p --renaming to current node - pass through
        for_ deps \d -> do --now `or` stack top with all `deps`
            bringToTop d d
            pop --first `OR` operand
            pop --second `OR` operand
            emit [ EISimple IOr ]
            push p --intermediate result
        for_ (drop 1 w) \_ -> do
            emit [ EISimple IDup ]
            push p --now result of this node is on stack
    go  Sink             = do
        isNode <- gets (elem p . esNodes)
        if isNode
        then bringToTop q p
        else do
            pop
            emit [ EISimple IDrop ]
--         nodes <- gets esNodes
--         if elem p nodes
--         then bringToTop q p
--         else do
--             pop
--             emit [ EISimple IDrop ]
    go (Source _a       ) = do
        push p
        emit [ EISimple ILdOn ]
    go  End              = do
        pop -- ????
        emit [ EISimple IDrop ]
    go (Device dev _a) = do
        bringToTop q q
        pop
        lift (emitDevice dev) >>= emit
        push p
    go (Jump   lbl  ) = do
        bringToTop q q
        emit [ EIJump lbl ]
        pop
    go (Cont   _continuation _a) = undefined
    go (Conn   _continuation) = undefined

    emit s = modify \st -> st { esCode = esCode st ++ s }

    bringToTop pp renameAs = do
        stk <- gets esStack
        case break ((==pp) . fst) stk of
            (_pre, []) -> undefined --not found
            ([], _) -> pop --pop current and push it with new name
            (pre, (v, _) : rest) -> do
                emit [ EISimple (IPick (length pre)) ]
                modify \st -> st { esStack = pre <> ((v, True) : rest) }
        push renameAs

    push v = modify \st -> st { esStack = (v, False) : esStack st }

    pop = do
        stk <- gets esStack
        case stk of
            _:stk' -> modify \st -> st { esStack = dropWhile snd stk' }
            [] -> undefined


data StackEmitState p w = StackEmitState
    { esStack :: [(p, Bool)] -- flag if used and can dropped
    , esSinks :: [p]
    , esNodes :: [p]
    , esCode :: w
    }

--------------------------------------------------------------------------------

-- literalFromInt2 :: (MonadError String m, Monad m) => Int -> m (V String)
-- literalFromInt2 i = return $ I $ fromIntegral i --TODO check range

generateStk2xx
    :: (Show addr, Show word, Show lbl, Eq lbl, MonadError String m, Monad m)
    => (dev -> Either String x) --TODO swap (Either String) for m
    -> (x -> [Instruction word addr])
    -> [(Maybe lbl, Cofree (Diagram Void dev lbl) DgExt)]
    -> m [ExtendedInstruction Int (Instruction word addr)]
generateStk2xx doOp emitDev ast = do
    ast'   <- for ast (traverse (mapOpsM (liftEither . doOp))) --FIXME remove liftEither
    ast''  <- for ast' (traverse (generateStk2' emitDev))
    ast''' <- liftEither $ resolveLabels ast'' 
    return ast'''

-- |Apply monadic action to every device
mapOpsM
    :: Monad m
    => (a -> m b) -- ^action to apply
    -> Cofree (Diagram c a s) p
    -> m (Cofree (Diagram c b s) p)
mapOpsM f (a :< n) = (a :<) <$> (mapDgA pure f pure n >>= traverse (mapOpsM f))

--------------------------------------------------------------------------------

wrapDevice3
    :: (Eq t, IsString t)
    => (Int -> Either String word)
    -> (addr -> Either String word)
    -> DeviceParser t ([(CellType, Operand t)], DeviceImpl word addr)
wrapDevice3 mkWord litFromAddr d
    = case lookup d (devices mkWord litFromAddr) of
        Just (DDesc _name ty impl)
            -> Right (if length ty > 1 then Mandatory else None
                    , \ops -> Right (zip (fmap snd ty) ops, impl))
        Nothing -> Left "device type unknown"

--------------------------------------------------------------------------------

mapDg :: (c -> c') -> (d -> d') -> (s -> s') -> Diagram c d s a -> Diagram c' d' s' a
mapDg z x y = runIdentity . mapDgA (pure . z) (pure . x) (pure . y)

mapDgA
    :: Applicative f
    => (c -> f c')
    -> (d -> f d')
    -> (s -> f s')
    -> Diagram c d s a
    -> f (Diagram c' d' s' a)
mapDgA z x y = f
    where
    f (Source   a) = pure $ Source     a
    f  Sink        = pure $ Sink
    f  End         = pure $ End
    f (Device d a) =        Device <$> x d <*> pure a
    f (Jump s    ) =        Jump   <$> y s
    f (Node     a) = pure $ Node       a
    f (Conn c    ) =        Conn   <$> z c
    f (Cont c   a) =        Cont   <$> z c <*> pure a
