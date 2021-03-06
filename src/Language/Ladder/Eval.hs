
module Language.Ladder.Eval where

import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
-- import Data.Traversable
-- import Data.Char (toUpper)
import Data.List
import Data.String

import Language.Ladder.Utils
import Language.Ladder.Types

--------------------------------------------------------------------------------

data CellType = Bit | TwoBits | Word
    deriving (Show, Read, Eq, Ord)
-- | TON | TOF
 
-- | Memory cell value, also represents its type and default value
data V addr
    = X !Bool
    | I !Int
    | A !addr
    deriving (Show, Read, Eq, Functor)

-- data V2 addr = T | F | I !Int | A !addr
--     deriving (Show, Read, Eq)

type Memory a = [(a, V a)]

--------------------------------------------------------------------------------

-- data EvResult st t = NeedJump st t | Done st | Failed String
data EvResult t = NeedJump (Either Int t) --  Failed String

data EvMem addr = EvMem
    { stTags :: [(addr, V addr)]
    }

data EvSt t = EvSt
    { stTmp :: [(DgExt, Bool)]
    }

evalM :: (Eq addr, Show addr, IsString t, Eq t, Show t)
    => [ (Maybe (Either Int t)
       , Cofree (Diagram c (DevType t, [Operand addr]) (Either Int t)) DgExt)]
    -> EvMem addr
    -> Either String (EvMem addr)
evalM blocks = execStateT (go blocks)
    where
    go ((_, x):xs)
        = evalRung x
        >>= \case
                Left (NeedJump lbl)
                      -> case break ((Just lbl==).fst) blocks of
                              (_, []) -> lift $ Left $ "label not found: " ++ show lbl
                              (_, target) -> go target
                Right _ -> go xs
    go [] = pure ()
                                          

evalRung :: (Eq addr, Show addr, IsString t, Eq t, Show t)
    => Cofree (Diagram c (DevType t, [Operand addr]) (Either Int t)) DgExt
    -> StateT (EvMem addr) (Either String) (Either (EvResult t) ([DgExt], EvSt t1))
evalRung ast = runExceptT (runStateT (traverseDiagram evalElem evalPost True ast) (EvSt []))
    where

    evalPost _ _ = pure ()

    evalElem q p = go
        where

        aaaargh (pp :< Node w) = do
            (||) <$> getTmp False pp <*> foldlM (\a b -> (a||) <$> aaaargh b) False w
--             xx <- for w aaaargh
--             t <- getTmp False pp
--             return $ or $ t : xx
        aaaargh _ = pure False

        go node@(Node _) = (q ||) <$> aaaargh (p :< node)
        go  Sink         = setTmp p q *> pure q
        go (Source _a  ) = pure True
        go  End          = pure q
        go (Device d _a) = evalDev d
        go (Jump   l   ) = if q then throwError (NeedJump l) else pure q
--         go (Cont  _c _a) = undefined
--         go (Conn  _c   ) = undefined
        go _ = undefined

        evalDev (d, a) = accEmitDev1 (d, a) -- accEmitDev1 (fmap toUpper <$> d, a)

        accEmitDev1 (Contact_ " ", [Var a   ]) = (q &&) <$> getBit a
        accEmitDev1 (Contact_ "/", [Var a   ]) = ((q &&) . not) <$> getBit a
        accEmitDev1 (Contact_ ">", [    a, b]) = (>) <$> getInt a <*> getInt b
        accEmitDev1 (Contact_ "<", [    a, b]) = (<) <$> getInt a <*> getInt b
        accEmitDev1 (Coil_    " ", [Var a   ]) = setBit a q
        accEmitDev1 (Coil_    "/", [Var a   ]) = setBit a (not q)
        accEmitDev1 (Coil_    "S", [Var a   ]) = if q then setBit a True else pure q
        accEmitDev1 (Coil_    "R", [Var a   ]) = if q then setBit a False else pure q
--         accEmitDev1 (Coil_    _d , _args     ) = undefined
        accEmitDev1 other = failure $ "unknown device:" ++ show other

        failure = lift . lift . lift . Left

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
                                                     _ -> failure "type mismatch"
        setBit n v = setMem stTags (\vv st -> st { stTags = vv}) n (X v) >> return v
        getInt (Lit i) = pure i
        getInt (Var n) = getMem stTags (I 0) n >>= \case
                                                     I i -> pure i
                                                     _ -> failure "type mismatch"

--------------------------------------------------------------------------------

getVariables
    :: (Eq addr, IsString t, Eq t)
    => [ (Maybe (Either Int lbl)
       , Cofree (Diagram c (DevType t, [Operand addr]) (Either Int t)) DgExt)]
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

--------------------------------------------------------------------------------
