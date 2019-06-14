#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Interpreter where

import Data.Foldable
import Data.Traversable
import Control.Monad.Writer.Strict
-- import Data.Function
import Data.List

-- import Language.Ladder.Zipper
-- import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Analysis

import Tooling

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

data Emit ca w d m = Emit
    { emLdOn :: m ()
    , emDrop :: m ()
    , emDevice :: d -> m ()
    , emBranch :: ca -> m () -- ???
    , emPick :: Int -> m ()
    , emDup :: m ()
    }

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

-- generate2 ::
--     ( Monad m0
--     , Show b0
--     , Show s1
--     , Show s0
--     , Eq b0
--     , Eq c
--     , Show c)
--     => ([Instruction String w] -> m0 ())
--     -> [Cofree (Diagram c (Op Operand s0) s1) b0]
--                       -> Cofree (Diagram c (Op Operand s0) s1) b0
--                       -> m0 [Cofree (Diagram c (Op Operand s0) s1) b0]

-- genStk :: _
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
                Just i -> bringToTop i
                Nothing -> error here --should not happen
            go (nd:stk) b --XXX not sure about nd on stack here

        f stk n = error $ show (here, stk, n)

    isConn p0 (_ :< Conn p1) = p0 == p1
    isConn _ _ = False

    bringToTop 0 = return ()
    bringToTop i = emit [IPick i]

emitBasicDevice d
    = case d of
        And  (Var addr) -> [ILdBit addr, IAnd]
        AndN (Var addr) -> [ILdBit addr, INot, IAnd]
        St   (Var addr) -> [IStBit addr]
        _               -> error $ show (here, d)

--------------------------------------------------------------------------------

generateStk2
    :: Cofree (Diagram () Dev String) DgExt
    -> IO [ExtendedInstruction String String Int]
generateStk2 ast' = do
    let ast = parseOps $ dropEnd ast'

    print (here, "-----------------------")
    print (here, "-----------------------")
    print (here, "-----------------------")

    --collapse nodes
    let (nodes, a0) = merge' ast

    --chop
    let Just (a1 ::[Cofree (Diagram () (Op Operand String) String) DgExt])
            = forest a0

    for_ a1 print
    print (here, "-----------------------")

    let a5 = cut1' a1
    let a6 = sortOn position a5
    let a7 = tsort2 nodes a6

    for_ a7 print
    print (here, "-----------------------")

    code <- execWriterT $ foldlM (genStk tell emitBasicDevice) [] a7
    for_ code print
    print (here, "-----------------------")

    return code

--------------------------------------------------------------------------------
