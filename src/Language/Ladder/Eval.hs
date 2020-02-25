{-# LANGUAGE MonadComprehensions #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Eval where

import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import Data.Char (toUpper)
-- import Data.String --IsString

import Language.Ladder.Utils
import Language.Ladder.Types
import Language.Ladder.Tooling
import Language.Ladder.Interpreter (Memory, V(..))

--------------------------------------------------------------------------------

evalTestVect1
    :: (Eq addr, Show addr)
    => [(Maybe lbl, Cofree (Diagram c (DevType String, [Operand addr]) t) DgExt)]
    -> [addr]
    -> TestVect addr
    -> Either (Memory addr, String) [[V addr]]
evalTestVect1 prog = evalTestVect getTag setTag step st0
    where
    getTag addr (EvMem m) = maybe undefined id $ lookup addr m -- addr -> EvMem addr -> V addr
    setTag (EvMem m) = EvMem . updateMemory m -- EvMem addr -> [(addr, V addr)] -> EvMem addr
    step = Right . eval prog
    st0 = EvMem [] -- []
-- updateMemory :: Eq addr => [(addr, V addr)] -> [(addr, V addr)] -> [(addr, V addr)]

--------------------------------------------------------------------------------

-- data EvResult st t = NeedJump st t | Done st | Failed String
data EvResult t = NeedJump t --  Failed String

data EvMem addr = EvMem
--     { stBits :: [(addr, Bool)]
--     , stInts :: [(addr, Int)]
--     }
    { stTags :: [(addr, V addr)]
    }

data EvSt t = EvSt
    { stTmp ::  [(DgExt, Bool)]
    }

-- eval :: (IsString d, Eq a0) => [(Maybe lbl
--             , Cofree (Diagram continuation (DevType d, [Operand a0]) t) DgExt)]
--                       -> EvMem a0
--                       -> EvMem a0
eval :: Eq addr
    => [(Maybe lbl, Cofree (Diagram c (DevType String, [Operand addr]) t) DgExt)]
    -> EvMem addr
    -> EvMem addr
eval blocks st0 =
    snd$flip runState st0 do
        for_ blocks \(_ , ast) -> do
            r <- blargh' ast
            case r of
                 Left (NeedJump _) -> undefined --TODO
                 Right _ -> return ()

-- blargh ast = runStateT (blargh' ast) (EvMem [] [])
-- blargh ast = runStateT (blargh' ast) (EvMem [])

-- blargh
--     :: (Ord p, Show p, Show l --     , Monad m
--     )
--     =>  Cofree (Diagram c d l) p
--     -> IO ([p]
--             , EvSt
--             )
blargh' ast
    = runExceptT (runStateT (traverseDiagram evalElem evalPost True ast) (EvSt []))
    where

    evalPost _ _ = return ()

    evalElem q p x = go x
        where

        go (Node   _w  ) = (q ||) <$> getTmp False p
        go  Sink         = setTmp p q *> pure q
        go (Source _a  ) = pure True
        go  End          = pure q
        go (Device d _a) = evalDev d
        go (Jump   l   ) = throwError (NeedJump l)
        go (Cont  _c _a) = undefined
        go (Conn  _c   ) = undefined

        evalDev (d, a) = accEmitDev1 (fmap toUpper <$> d, a)

        accEmitDev1 (Contact_ " ", [Var a]) = (q &&) <$> getBit a
--         accEmitDev1 (Contact_ "/", [Var a]) = pure undefined
        accEmitDev1 (Contact_ ">", [a, b]) = (>) <$> getInt a <*> getInt b
--         accEmitDev1 (Contact_ "<", _args) = undefined
--         accEmitDev1 (Contact_ d, _args) = error $ show d
        accEmitDev1 (Coil_ " ", [Var a]) = setBit a q
--         accEmitDev1 (Coil_ "/", [Var a]) = pure undefined
--         accEmitDev1 (Coil_ "S", [Var a]) = pure undefined
--         accEmitDev1 (Coil_ "R", [Var a]) = undefined
        accEmitDev1 (Coil_ _d, _args) = undefined
        accEmitDev1 _ = undefined

        getTmp d pp = getTag' stTmp d pp
        setTmp pp v = modify $ \st -> st { stTmp = (pp, v) : stTmp st}

        getTag' f d n = gets (maybe d id . lookup n . f)



        setMem f g (n) v' = lift do
            m <- gets f
            case break ((n==).fst) m of
                 (ys, _xx:xs) -> modify $ g (ys ++ [(n, v')] ++ xs)
                 (xs, []) -> modify $ g ((n, v') : xs)

        getMem f d n = lift do getTag' f d n

        getBit n = getMem stTags undefined n >>= \case
                                                     X b -> pure b
                                                     _ -> undefined
        setBit n v = setMem stTags (\vv st -> st { stTags = vv}) n (X v) >> return v
        getInt (Lit i) = pure i
        getInt (Var n) = getMem stTags (I 0) n >>= \case
                                                     I i -> pure i
                                                     _ -> undefined

