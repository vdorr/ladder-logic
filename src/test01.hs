{-# OPTIONS_GHC -Wunused-imports  -Wall #-}
{-# LANGUAGE CPP, TupleSections, FlexibleContexts, ScopedTypeVariables, BlockArguments #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Foldable
-- import Data.Void

-- import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Simple
-- import Language.Ladder.Interpreter

import Language.Ladder.Utils

import Control.Monad.State
-- import qualified Data.Map as M
-- import Data.Bifunctor
import Data.List
import Data.Function
import Data.Proxy

--------------------------------------------------------------------------------

-- testAst :: Cofree (Diagram Void (Dev String) String) DgExt -> IO ()
-- testAst ast' = do
--     generateStk2 ast'
--     return ()

--------------------------------------------------------------------------------

{-
(1) processing top->down, left->right
(2) processing is postponed (made out of abovementioned order)
    in horizontal direction when node is fed from yet unprocessed device
    in vertical direction when yet unprocessed line has to be stepped over
        vertical means when going down the rail from node

(3) how to recognize when i can complete postponed work?
(4) postponing happens only at nodes

(5) when node has unprocessed predeccessor?
    when it has outgoing branch with node that coincides on position with some 'Sink'

(6a) both position-priority and data-dependency need to be checked only at 'Node' element
(6b) posponed actions are checked after 'Sink' (data and position deps)
    or 'Node' (position deps) is evaluated
-}

data PP m a = PP
    { ppPos :: DgExt
    , ppNodes :: [DgExt]
    , ppCont :: a -> StateT (TraverseState m a) m ()
    }

-- 'postponed' is appended when -see above
data TraverseState m a = TraverseState
    { postponedUntil :: [PP m a] -- location after which we can run cont, cont
    , unevaluatedNodes :: [DgExt]
    , evaluatedSinks :: [DgExt]
    }

collectSinks (p :< Sink) = modify (<> pure p)
collectSinks (_ :< other) = for_ other collectSinks

collectNodes (p :< Node w) = modify (<> pure p) *> for_ w collectNodes
collectNodes (_ :< other) = for_ other collectNodes

collectNodes' ast = execState (collectNodes ast) []


checkDataDep sinks x 
    | (p :< Node w) <- x, elem p sinks = return p 
    | otherwise                        = mempty
-- checkDatatDep _ _ = Nothing

--------------------------------------------------------------------------------

hello ast = execStateT (go ast) (TraverseState [] unEvNodes [])

    where

    go x@(p :< Node w) = do

--check data dependency
--         let deps = foldMap (checkDataDep sinks) w
--         evaluated <- gets evaluatedSinks
--         let dataDeps = deps \\ evaluated
        dataDeps <- getDataDeps w

--         case dataDeps of
--              [] -> do --no unevaluated dependecies, good, proceed
-- --                 undefined 
--                 return ()
--              deps -> do --postpone
--                  liftIO $ print $ maximum deps
-- --                  undefined
--                  return ()

--check if i need to evaluate other line first
--         rr <- gets unevaluatedNodes
--         rr <- getUnevaluatedAndNotPostponedNodes
--         let locationDeps = filter (< p) rr
        locationDeps <- getUnevaluatedAndNotPostponedNodesAt p

--         case locationDeps of
--             [] -> do -- good, emit
-- --                 undefined
--                 return ()
--             d -> do -- postpone until `maximum d`
--                 let r = maximum d
--                 liftIO $ print r
-- --                 undefined
--                 return ()

        case dataDeps <> locationDeps of
            [] -> do
--                 modify $ \st -> st {unevaluatedNodes=delete p (unevaluatedNodes st)}
                markNodeAsEvaluated p
                emit x
                runPostponed p
            deps -> do
                let r = maximum deps
                liftIO $ print (here, "POSTPONED", p, "until", r)
                postpone x r
--                 undefined
    go x@(p :< Sink) = do
        modify $ \st -> st {evaluatedSinks=pure p <> evaluatedSinks st}
        emit x
        runPostponed p

    go othr@(_ :< other) = emit othr --for_ other go

--look for unevaluted nodes on which node depends
    getDataDeps w = do
        let deps = foldMap (checkDataDep sinks) w
        evaluated <- gets evaluatedSinks
        return $ deps \\ evaluated

    markNodeAsEvaluated p = modify $ \st -> st {unevaluatedNodes=delete p (unevaluatedNodes st)}

    unEvNodes = execState (collectNodes ast) []

    --hate this hack :(
    getUnevaluatedAndNotPostponedNodes = do
--         rr <- gets unevaluatedNodes
--         q <- gets (foldMap ppNodes . postponedUntil)
--         return $ rr \\ q
        (\\) <$> gets unevaluatedNodes <*> gets (foldMap ppNodes . postponedUntil)

    getUnevaluatedAndNotPostponedNodesAt p
        = filter (< p) <$> getUnevaluatedAndNotPostponedNodes

    postpone ast1 p = do
        modify $ \st
            -> st {postponedUntil
                    =pure (PP p (collectNodes' ast1) (\() -> go ast1)) <> postponedUntil st}

    runPostponed p = do
        q <- gets postponedUntil
        let qq = filter ((p==).ppPos) q
        modify $ \st
            -> st { postponedUntil = deleteFirstsBy (on (==) ppPos) (postponedUntil st) qq}
--         for_ qq ppCont
        for_ qq $ \pp -> do
            liftIO $ print ("COMPLETING >>")
            ppCont pp ()
            liftIO $ print ("<< COMPLETED")

    sinks = execState (collectSinks ast) []
--     hasDep p = elem p sinks --or "intersectsWithSink"
        --FIXME should be according to (5)
        --FIXME return location

--     allLinesAboveEvaluated p = undefined
        --FIXME return location of end of line sink
        -- look for element of unevaluatedNodes strictly northeast of p
        -- or, even better, largest of all lying northeast of p

    emit (p :< Sink) = do
--         modify $ \st -> st {evaluatedSinks=p:evaluatedSinks st}
        liftIO $ print ("sink", p)
--         runPostponed p
--         return ()
    emit (p :< Source a) = liftIO (print ("src", p)) *> go a
    emit (p :< End      ) = liftIO (print ("end", p))
    emit (p :< Device device a) = liftIO (print ("dev", device, p)) *> go a
    emit (p :< Jump   _label) = liftIO (print ("jump", p)) *> undefined
    emit (p :< Node   w) = liftIO (print ("node", p)) *> for_ w go -- *> runPostponed p
    emit (p :< Cont   _continuation _a) = undefined
    emit (p :< Conn   _continuation) = undefined

--------------------------------------------------------------------------------

-- traverseDiagram
--     :: Monad m
--     => (DgExt -> Diagram continuation device label () -> m a)
--     -> Cofree (Diagram continuation device label) DgExt
--     -> m (TraverseState m)
traverseDiagram emit q0 ast = execStateT (go q0 ast) (TraverseState [] unEvNodes [])

    where

    go q x@(p :< Node w) = do
        dataDeps <- getDataDeps w
        locationDeps <- getUnevaluatedAndNotPostponedNodesAt p
        case dataDeps <> locationDeps of
            [] -> do
                markNodeAsEvaluated p
                q' <- emit' q x
                runPostponed q' p
                for_ w (go q')
            deps -> do
                let r = maximum deps --pospone until most distant dependecy
                postpone x r q
    go q x@(p :< Sink) = do
        markSinkAsEvaluated p
        q' <- emit' q x
        runPostponed q' p
    go q othr@(_ :< x) = emit' q othr >>= \q' -> for_ x (go q')

--     emit' q (p :< x) = lift $ emit q p $ fmap (const ()) x
    emit' q (p :< x) = lift $ emit q p x

--look for unevaluted nodes on which node depends
--XXX why evaluatedSinks? why not all sinks?
--returns not yet evaluated dependencies
--     getDataDeps w = collectIncidentSinks <$> gets evaluatedSinks <*> pure w
    getDataDeps w = do
        let deps = foldMap (checkDataDep sinks) w
        evaluated <- gets evaluatedSinks
        return $ deps \\ evaluated

--     collectIncidentSinks :: _
--     collectIncidentSinks sinks' w =
--         let deps = foldMap (checkDataDep sinks) w
--             in deps \\ sinks'

    markSinkAsEvaluated p = modify $ \st -> st {evaluatedSinks=pure p <> evaluatedSinks st}
    markNodeAsEvaluated p = modify $ \st -> st {unevaluatedNodes=delete p (unevaluatedNodes st)}

    unEvNodes = execState (collectNodes ast) []

    --hate this hack :(
    getUnevaluatedAndNotPostponedNodes
        = (\\) <$> gets unevaluatedNodes <*> gets (foldMap ppNodes . postponedUntil)

    getUnevaluatedAndNotPostponedNodesAt p
        = filter (< p) <$> getUnevaluatedAndNotPostponedNodes

    postpone ast1 p q
        = modify \st
            -> st {postponedUntil
            =pure (PP p (collectNodes' ast1) (\q -> go q ast1)) <> postponedUntil st}

    runPostponed qqq p = do
        q <- gets postponedUntil
        let qq = filter ((p==).ppPos) q
        modify $ \st
            -> st { postponedUntil = deleteFirstsBy (on (==) ppPos) (postponedUntil st) qq}
        for_ qq \pp -> do
            ppCont pp qqq

    sinks = execState (collectSinks ast) []

-- hndlCnt f
--     = gets esCnt
--     >>= \cnt -> f (cnt)
--         >> modify (\st -> st { esCnt = 1 + cnt})
--         >> return (1 + cnt)

hndlCnt f = do
    cnt <- gets esCnt
    f cnt
    modify (\st -> st { esCnt = 1 + cnt})
    return (1 + cnt)

emitPrint q p (Node   w       ) = hndlCnt \cnt -> liftIO (print ("node", p, q, cnt))
emitPrint q p  Sink             = hndlCnt \cnt -> liftIO (print ("sink", p, q, cnt))
emitPrint q p (Source a       ) = hndlCnt \cnt -> liftIO (print ("src", p, q, cnt))
emitPrint q p  End              = hndlCnt \cnt -> liftIO (print ("end", p, q, cnt))
emitPrint q p (Device device a)
    = hndlCnt \cnt -> liftIO (print ("dev", device, p, q, cnt))
emitPrint q p (Jump   _label  ) = hndlCnt \cnt -> liftIO (print ("jump", p, q, cnt))
emitPrint q p (Cont   _continuation _a) = undefined
emitPrint q p (Conn   _continuation) = undefined

--------------------------------------------------------------------------------

em0 q p (Node   []      ) = hndlCnt \_ -> return () -- "optimization"
em0 q p (Node   w       ) = hndlCnt \cnt -> do
--     liftIO (print ("node", p, q, cnt))
    sinks <- gets esSinks
    stk <- gets esStack
    let deps :: [DgExt] = foldMap (checkDataDep sinks) w
--now `or` stack top with all `deps`
    for_ deps \d -> do
        let Just idx = findIndex (==d) stk
        liftIO $ print (">>>>> PICK ", idx)
        liftIO $ print (">>>>> OR", p, d)
    push p --now result of this node is on stack
    liftIO $ print ("node", length w, length deps)
em0 q p  Sink             = hndlCnt \cnt -> do
    push p
    liftIO (print ("sink", p, q, cnt))
em0 q p (Source a       ) = hndlCnt \cnt -> do
    liftIO $ print (">>>>> PUSH #1")
    liftIO (print ("src", p, q, cnt))
-- em0 q p  End              = hndlCnt \cnt -> liftIO (print ("end", p, q, cnt))
em0 q p  End              = hndlCnt \cnt -> return ()
em0 q p (Device device a) = hndlCnt \cnt -> do
    pop
    push p
    liftIO (print (">>>>> EVAL", device))
    liftIO (print ("dev", device, p, q, cnt))
em0 q p (Jump   _label  ) = undefined -- hndlCnt \cnt -> liftIO (print ("jump", p, q, cnt))
em0 q p (Cont   _continuation _a) = undefined
em0 q p (Conn   _continuation) = undefined

push x = do
    stk <- gets esStack
    modify \st -> st { esStack = x:stk}

pop :: Monad m => StateT EmitState m ()
pop = do
    stk <- gets esStack
    case stk of
        _:stk' -> modify \st -> st -- { esStack = stk' }
        [] -> undefined


data EmitState = EmitState
    { esStack :: [DgExt]
    , esCnt :: Integer
    , esSinks :: [DgExt]
    }

-- blargh ast = runStateT (traverseDiagram go ast) (EmitState [])
-- 
--     where
-- 
--     go p  Sink             = liftIO (print ("sink", p))
--     go p (Source a       ) = liftIO (print ("src", p))
--     go p  End              = liftIO (print ("end", p))
--     go p (Device device a) = liftIO (print ("dev", device, p))
--     go p (Jump   _label  ) = liftIO (print ("jump", p)) *> undefined
--     go p (Node   w       ) = liftIO (print ("node", p))
--     go p (Cont   _continuation _a) = undefined
--     go p (Conn   _continuation) = undefined

--------------------------------------------------------------------------------

main :: IO ()
main = do
    [file] <- getArgs
    src <- TIO.readFile file
    case stripPos3 <$> runLexer src of
        Left err -> TIO.putStrLn err
        Right lxs -> do

            let lxs' = dropWhitespace2 lxs
            let blocks = labeledRungs lxs'

            print (here, "--------------------------------------------------")
            TIO.putStrLn src
            print (here, "--------------------------------------------------")

            forM_ blocks $ \(lbl, lxs'') -> do
                print (here, lbl)
                let zp = mkDgZp lxs''
                for_ (toList zp) (print . (here,))
                case runLadderParser deviceThing ladderLiberal lxs'' of
                    Left err -> print (here, err)
                    Right (ast1, zp1) -> do
                        print (here, "--------------------------------------------------")
                        for_ (toList zp1) (print . (here,))
                        print (here, "--------------------------------------------------")
                        print (here, ast1)
                        putStrLn ""
                        print (here, "--------------------------------------------------")
                        putStrLn ""
--                         sinks ast1
                        runStateT
                            (traverseDiagram em0 0 ast1)
                            (EmitState [] 0 (execState (collectSinks ast1) []))
--                         u <- traverseDiagram emitPrint 0 ast1
--                         print $ length $ postponedUntil u
--                         print $ length $ unevaluatedNodes u
--                         print $ length $ evaluatedSinks u
                        return ()
    where
--     deviceThing = wrapDevice3 (pure . I) (pure . A)
    deviceThing = wrapDeviceSimple
