#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Eval where

import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

import Language.Ladder.Utils
import Language.Ladder.Types

--------------------------------------------------------------------------------

-- data EvResult st t = NeedJump st t | Done st | Failed String
data EvResult t = NeedJump t --  Failed String

data EvMem t = EvMem
    { stBits :: [(t, Bool)]
    , stInts ::  [(t, Int)]
    }

data EvSt t = EvSt
    {
--         stBits :: [(t, Bool)]
--     , stInts ::  [(t, Int)]
--     , 
    stTmp ::  [(DgExt, Bool)]
    }

eval :: Eq a0 => [(Maybe lbl
            , Cofree (Diagram continuation (DevType String, [Operand a0]) t) DgExt)]
                      -> EvMem a0
                      -> EvMem a0
eval blocks st0 =
    (snd$flip runState st0 do
        for_ blocks \(_ , ast) -> do
            r <- blargh' ast
            case r of
                 Left (NeedJump _) -> undefined --TODO
                 Right _ -> return ()
    ) -- :: EvMem String
--     undefined

blargh ast
    = runStateT
        (blargh' ast)
        (EvMem [] [])

-- blargh
--     :: (Ord p, Show p, Show l --     , Monad m
--     )
--     =>  Cofree (Diagram c d l) p
--     -> IO ([p]
--             , EvSt
--             )
blargh' ast
    = runExceptT (runStateT (traverseDiagram evalElem evalPost True ast) (EvSt []))
--     = runStateT
--         (runExceptT (runStateT (traverseDiagram evalElem evalPost True ast) (EvSt [])))
--         (EvMem [] [])
    where
--     (nodes, sinks) = collectNodesAndSinks ast

    evalPost _ _ = return ()

    evalElem (q::Bool) p x = go x
        where

--         go (Node   []      ) = do
--             liftIO $ print (here)
--             return undefined
        go (Node   _w       ) = do
--             liftIO $ print (here)
            q' <- getTmp' False p
            return $ q || q' 
        go  Sink             = do
            setTmp p q
            return q
        go (Source _a       ) = do
            return True
        go  End              = do
            return q
        go (Device device _a) = do
            accEmitDev1 device
        go (Jump   lbl  ) = do
            throwError $ NeedJump lbl
        go (Cont   _continuation _a) = undefined
        go (Conn   _continuation) = undefined


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

        getBit n = getMem stBits undefined n
        setBit n v = setMem stBits (\vv st -> st { stBits = vv}) n v >> return v
        getInt (Var n) = getMem stInts 0 n
        getInt (Lit i) = pure i
--         setInt n v = setTag stInts (undefined) n v

        getTmp' d pp = getTag' stTmp d pp
        setTmp pp v = modify $ \st -> st { stTmp = (pp, v) : stTmp st}

        setMem f g (n) v' = lift do
            m <- gets f
            case break ((n==).fst) m of
                 (ys, _xx:xs) -> modify $ g (ys ++ [(n, v')] ++ xs)
                 (xs, []) -> modify $ g ((n, v') : xs)

        getMem f d n = lift do getTag' f d n

        getTag' f d n = do
            m <- gets f
            case lookup n m of
                 Just vv -> return vv
                 _ -> return d

-- -- blargh
-- --     :: (Ord p, Show p, Show l --     , Monad m
-- --     )
-- --     =>  Cofree (Diagram c d l) p
-- --     -> IO ([p]
-- --             , EvSt
-- --             )
-- blargh ast@(q0 :< _)
--     = (runStateT
--         (traverseDiagram (evalElem) evalPost q0 ast)
--         (EvSt []) --(AccuEmitState q0 sinks nodes [] [])
--         ) 
--     where
--     (nodes, sinks) = collectNodesAndSinks ast
-- 
--     evalElem q p x = go x
--     evalPost _ = undefined
-- 
-- 
--     go (Node   []      ) = do
--         liftIO $ print (here)
--         return ()
--     go (Node   w       ) = do
--         liftIO $ print (here)
--         undefined
--     go  Sink             = do
--         undefined
--     go (Source _a       ) = do
--         undefined
--     go  End              = do
--         undefined
--     go (Device device _a) = do
--         undefined
--     go (Jump   label  ) = do
--         undefined
--     go (Cont   _continuation _a) = undefined
--     go (Conn   _continuation) = undefined
