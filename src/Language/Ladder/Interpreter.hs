#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Interpreter where

import Data.Foldable
import Data.Traversable
import Control.Monad.Writer.Strict
-- import Data.Function
import Data.List
-- import Data.Int
-- import Data.Word
-- import Data.Bifunctor

import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Analysis

--------------------------------------------------------------------------------

-- |Memory cell value, also represents its type and default value
data V
    = X Bool 
    | I Int
    deriving (Show, Read, Eq)

data Op n s
    = And n -- wire out <- wire in and memory cell
    | AndN n
    | Ld    -- ^ set wire state same as argument
    | On    -- ^ set wire state to #on
    | St n
--     | StN | StOn | StOff
    | Jmp s
    | Cmp CmpOp n n
    -- | FB String [(String, D)] [(String, D)]
    deriving Show

data CmpOp = Lt | Gt | Lte | Gte | Eq | NEq
    deriving Show

--------------------------------------------------------------------------------

data ExtendedInstruction ca a w
    = EIJump ca
    | EISimple (Instruction a w)
--TODO TODO TODO
    | EIReturn
    deriving Show

data Instruction a w
    = ITrap --invoke debugger
--     | ISysRq
    | ILdOn -- push #1 onto wire stack {- w: -- #1 -}
    | IDup -- coudl be replaced by IPick 0, dup value on top of wire stack {- w: x -- x x -}
    | IPick  Int -- push wire stack value at index onto wire stack {- w: -- x -}
    | IDrop --drop value from wire stack {- w: x -- -}

    | ILdBit a -- push bit from address onto wire stack
    | IStBit a -- dtto for store

--     | IJump  String
    | IAnd -- and two values on wire stack, push result back
    | IOr -- dtto, or
    | INot -- negate value on top of wire stack
--     | IXor

    | ILdCnA w {- push int const onto argument stack, a: -- l -}
    | ILdM {- a: size addr -- <value> -}
    | IStM

--     | IOp Operator --instead of lt,gt,etc
    | IEq -- compare two value on arg stack and push result onto wire stack
    | ILt
    | IGt
    deriving Show

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

--get rid of text
resolveLabels
    :: (Show lbl, Eq lbl)
    => [(Maybe lbl, [ExtendedInstruction lbl a w])]
    -> Either String [ExtendedInstruction Int a w]
resolveLabels l = for (foldMap snd l) g
    where
    l' = fmap (fmap length) l

    (_, l'') = foldl f (0, []) l'
    f (acc, xs) (lbl, blkLen) = (acc + blkLen, (lbl, acc) : xs)

    g (EIJump lbl) = case lookup (Just lbl) l'' of
                       Just a -> Right (EIJump a)
                       Nothing -> Left $ show (here, lbl)
    g (EISimple i) = Right (EISimple i)
    g  EIReturn    = Right EIReturn

genStk
    :: Monad m
    => ([ExtendedInstruction String a w]
    -> m ())
    -> (Op Operand String -> [Instruction a w])
    -> [Cofree (Diagram DgExt (Op Operand String) String) DgExt]
    -> Cofree (Diagram DgExt (Op Operand String) String) DgExt
    -> m [Cofree (Diagram DgExt (Op Operand String) String) DgExt]
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

        f stk n = error $ show (here, stk, n)

    isConn p0 (_ :< Conn p1) = p0 == p1
    isConn _   _             = False

    bringToTop 0 = return ()
    bringToTop i = emit [IPick i]

emitBasicDevice d
    = case d of
        And  (Var addr) -> [ILdBit addr, IAnd]
        AndN (Var addr) -> [ILdBit addr, INot, IAnd]
        St   (Var addr) -> [IStBit addr]
        _               -> error $ show (here, d)

--------------------------------------------------------------------------------

verbose1 = False

--FIXME IO
generateStk2
    :: Cofree (Diagram () Dev String) DgExt
    -> IO [ExtendedInstruction String String Int]
generateStk2 ast' = do
    let ast = parseOps $ dropEnd ast'
    --collapse nodes
--     let (nodes, a0) = merge' ast
    let (nodes, a0) = repositionSinks nodes <$> merge' ast
    --chop
    let Just a1 = forest a0
    let a5 = cut1' a1
    let a6 = sortOn position a5
    
--     let a6' :: [(Deps DgExt, Cofree (Diagram DgExt (Op Operand String) String) DgExt)]
--                         = fmap (\n -> (dependencies2 nodes n, n)) a6
    
--     let a7 = tsort2 nodes a6
    let a7 = tsort3 a6

    code <- execWriterT $ foldlM (genStk tell emitBasicDevice) [] a7 --need failure here
    when verbose1 $ do
        print (here, "-----------------------")
        for_ a1 print

        print (here, "nodes", "-----------------------")
        for_ nodes print

        print (here, "after sort on position", "-----------------------")
        for_ a6 print

--         print (here, "same, but with deps", "-----------------------")
--         for_ a6' $ \(dep, ast) -> do
--             print dep 
--             print $ dependencies ast
--             putStr "  "
--             print ast
        print (here, "after tsort2", "-----------------------")
        for_ a7 print
        print (here, "-----------------------")
        for_ (code :: [ExtendedInstruction String String Int]) print
        print (here, "-----------------------")
    return code

--------------------------------------------------------------------------------

parseOps
    :: Cofree (Diagram c Dev s) p
    -> Cofree (Diagram c (Op Operand s) s) p
parseOps (a :< n) = a :< fmap parseOps (mapDg id f id n)
    where
    f (Dev "[ ]" [n]   ) = And  n
    f (Dev "[/]" [n]   ) = AndN  n
    f (Dev "[>]" [a, b]) = Cmp Gt a b
    f (Dev "( )" [n]   ) = St n
    f _                  = error here

--------------------------------------------------------------------------------

--wire stack count, wire stack, arg stack, memory???
-- type ItpSt2 = (Word8, Word16, [Int16], ([Word8], [Int16]))

-- eval2 :: ItpSt2 -> Instruction Int Int16 -> Either (ItpSt2, String) ItpSt2
-- eval2 = undefined

type Program = [ExtendedInstruction Int String Int]

-- data Trigger = Periodic Int | Memory String
data Task = Task { nextRun, priority, period :: Int, program :: Program }

type ItpSt3 = (Clock, [Task], Memory)
type Period = Int
type Clock = Int
type Prio = Int

makeItpSt3 :: Memory -> [(Period, Prio, Program)] -> ItpSt3
makeItpSt3 m tasks = (0, fmap (\(per, pri, pro) -> Task 0 pri per pro) tasks, m)

run
    :: ItpSt3
    -> Either (ItpSt, String) ItpSt3 --FIXME error should return also offending task
run (clk, tasks, st0)
    = (clk + 1, waiting ++ nextRound,)
    <$> foldlM execute st0 run''
    where
    (runnableNow, waiting) = partition ((clk==).nextRun) tasks
    run' = sortOn priority runnableNow
    run'' = fmap program run'
    nextRound = fmap (\tsk@Task{..} -> tsk { nextRun = clk + period }) run'

--------------------------------------------------------------------------------

-- execute'
--     :: ItpSt
--     -> [(Maybe String, [ExtendedInstruction String String Int])]
--     -> Either (ItpSt, String) ItpSt
-- execute' st0 prog = bimap (st0,) id (resolveLabels prog) >>= execute st0

execute
    :: Memory
    -> [ExtendedInstruction Int String Int]
    -> Either (ItpSt, String) Memory
execute mem0 prog = (\(_, _, m) -> m) <$> f prog ([], [], mem0)
    where

    f []               st = return st
    f (EIReturn   : _) st = return st
    f (EIJump lbl : _) st = nextLabel lbl st
    f (EISimple i : p) st = eval st i >>= f p

    nextLabel lbl = f (drop lbl prog)

--------------------------------------------------------------------------------

type Memory = [(String, V)]

-- data ItSt = ItSt [Bool] [V] [(String, V)]
type ItpSt = ([Bool], [V], Memory)

eval :: ItpSt -> Instruction String Int -> Either (ItpSt, String) ItpSt
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

    f _                  i          = error $ show (here, i)

--     f    (ItSt ws     os         m) (ILdArg o) = pure $ ItSt ws (o:os) m
--     f st@(ItSt ws     (Var n:os) m)  ILdM
--         | Just v <- lookup n m                 = undefined --pure $ ItSt ws os m
--         | otherwise                            = Left (st, "var not found")

--------------------------------------------------------------------------------
