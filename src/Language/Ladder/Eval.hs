#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Eval where

import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import Data.Char (toUpper)
-- import Data.String --IsString

import Language.Ladder.Utils
import Language.Ladder.Types

--------------------------------------------------------------------------------

-- data EvResult st t = NeedJump st t | Done st | Failed String
data EvResult t = NeedJump t --  Failed String

data EvMem t = EvMem
    { stBits :: [(t, Bool)]
    , stInts :: [(t, Int)]
    }

data EvSt t = EvSt
    { stTmp ::  [(DgExt, Bool)]
    }

-- eval :: (IsString d, Eq a0) => [(Maybe lbl
--             , Cofree (Diagram continuation (DevType d, [Operand a0]) t) DgExt)]
--                       -> EvMem a0
--                       -> EvMem a0
eval :: (Eq a0) => [(Maybe lbl
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
    )

blargh ast = runStateT (blargh' ast) (EvMem [] [])

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

    evalElem (q::Bool) p x = go x
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

        getBit n = getMem stBits undefined n
        setBit n v = setMem stBits (\vv st -> st { stBits = vv}) n v >> return v
        getInt (Var n) = getMem stInts 0 n
        getInt (Lit i) = pure i
--         setInt n v = setTag stInts (undefined) n v

        getTmp d pp = getTag' stTmp d pp
        setTmp pp v = modify $ \st -> st { stTmp = (pp, v) : stTmp st}

        setMem f g (n) v' = lift do
            m <- gets f
            case break ((n==).fst) m of
                 (ys, _xx:xs) -> modify $ g (ys ++ [(n, v')] ++ xs)
                 (xs, []) -> modify $ g ((n, v') : xs)

        getMem f d n = lift do getTag' f d n

        getTag' f d n = gets (maybe d id . lookup n . f)
--         getTag' f d n = do
--             m <- gets f
--             case lookup n m of
--                  Just vv -> return vv
--                  _ -> return d

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
