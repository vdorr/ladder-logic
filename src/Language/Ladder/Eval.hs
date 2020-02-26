
module Language.Ladder.Eval where

import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import Data.Traversable
import Data.Char (toUpper)
-- import Data.String --IsString
import Data.List

import Language.Ladder.Utils
import Language.Ladder.Types
-- import Language.Ladder.Tooling
-- import Language.Ladder.Interpreter (Memory, V(..), CellType(..))

--------------------------------------------------------------------------------

data CellType = Bit | TwoBits | Word
    deriving (Show, Read, Eq, Ord)
-- | TON | TOF
 
-- | Memory cell value, also represents its type and default value
data V addr
    = X !Bool
    | I !Int
    | A !addr
    deriving (Show, Read, Eq)

-- data V2 addr = T | F | I !Int | A !addr
--     deriving (Show, Read, Eq)

type Memory a = [(a, V a)]

--------------------------------------------------------------------------------

-- data EvResult st t = NeedJump st t | Done st | Failed String
data EvResult t = NeedJump t --  Failed String

data EvMem addr = EvMem
    { stTags :: [(addr, V addr)]
    }

data EvSt t = EvSt
    { stTmp :: [(DgExt, Bool)]
    }

eval :: (Eq addr, Show addr)
    => [(Maybe lbl, Cofree (Diagram c (DevType String, [Operand addr]) t) DgExt)]
    -> EvMem addr
    -> EvMem addr
eval blocks st0 =
    snd $ flip runState st0 do
        for_ blocks \(_ , ast) -> do
            r <- evalRung ast
            case r of
                 Left (NeedJump _) -> undefined --TODO
                 Right _ -> return ()

evalRung :: (Eq addr, Show addr, Monad m)
    => Cofree (Diagram c (DevType String, [Operand addr]) t) DgExt
    -> StateT (EvMem addr) m (Either (EvResult t) ([DgExt], EvSt t1))
evalRung ast = runExceptT (runStateT (traverseDiagram evalElem evalPost True ast) (EvSt []))
    where

    evalPost _ _ = pure ()

    evalElem q p = go
        where

        aaaargh (pp :< Node w) = do
            xx <- for w aaaargh
            t <- getTmp False pp
            return $ or $ t : xx
        aaaargh _ = pure False

        go node@(Node _) = (q ||) <$> aaaargh (p :< node)
        go  Sink         = setTmp p q *> pure q
        go (Source _a  ) = pure True
        go  End          = pure q
        go (Device d _a) = evalDev d
        go (Jump   l   ) = throwError (NeedJump l)
        go (Cont  _c _a) = undefined
        go (Conn  _c   ) = undefined

        evalDev (d, a) = accEmitDev1 (fmap toUpper <$> d, a)

        accEmitDev1 (Contact_ " ", [Var a   ]) = (q &&) <$> getBit a
        accEmitDev1 (Contact_ "/", [Var a   ]) = ((q &&) . not) <$> getBit a
        accEmitDev1 (Contact_ ">", [    a, b]) = (>) <$> getInt a <*> getInt b
        accEmitDev1 (Contact_ "<", [    a, b]) = (<) <$> getInt a <*> getInt b
        accEmitDev1 (Coil_    " ", [Var a   ]) = setBit a q
        accEmitDev1 (Coil_    "/", [Var _a   ]) = undefined
        accEmitDev1 (Coil_    "S", [Var _a   ]) = undefined
        accEmitDev1 (Coil_    "R", [Var _a   ]) = undefined
        accEmitDev1 (Coil_    _d , _args     ) = undefined
        accEmitDev1 _                          = undefined

        getTmp d pp = getTag' stTmp d pp
        setTmp pp v = modify $ \st -> st { stTmp = (pp, v) : stTmp st}

        getTag' f d n = gets (maybe d id . lookup n . f)

        setMem f g (n) v' = lift do
            m <- gets f
            case break ((n==).fst) m of
                 (ys, _xx:xs) -> modify $ g (ys ++ [(n, v')] ++ xs)
                 (xs, []) -> modify $ g ((n, v') : xs)

        getMem f d n = lift do getTag' f d n

        getBit n = getMem stTags (error (show n)) n >>= \case
                                                     X b -> pure b
                                                     _ -> undefined
        setBit n v = setMem stTags (\vv st -> st { stTags = vv}) n (X v) >> return v
        getInt (Lit i) = pure i
        getInt (Var n) = getMem stTags (I 0) n >>= \case
                                                     I i -> pure i
                                                     _ -> undefined

getVariables
    :: Eq addr
    => [(Maybe lbl, Cofree (Diagram c (DevType String, [Operand addr]) t) DgExt)]
    -> [(CellType, addr)]
getVariables ast = nub $ snd $ runState (for_ ast (go' . snd)) []
    where

    go' (_ :< a) = go a
    go (Node   w   ) = for_ w go'
    go (Source a   ) = go' a
    go (Device d a ) = evalDev d *> go' a
    go (Cont  _c _a) = undefined
--     go (Conn  _c   ) = undefined
    go  _           = pure ()

    evalDev (Contact_ ">", args) = ints args
    evalDev (Contact_ "<", args) = ints args
    evalDev (Contact_ _  , args) = bits args
    evalDev (Coil_    _  , args) = bits args

    bits args = modify ( [ (Bit, addr) | Var addr <- args] ++ )
    ints args = modify ( [ (Word, addr) | Var addr <- args] ++ )

