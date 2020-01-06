{-# OPTIONS_GHC -Wunused-imports  -Wall #-}
{-# LANGUAGE CPP, TupleSections, FlexibleContexts, ScopedTypeVariables #-}
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

data PP m = PP
    { ppPos :: DgExt
    , ppNodes :: [DgExt]
    , ppCont :: StateT (EmitState m) m ()
    }

-- 'postponed' is appended when -see above
data EmitState m = EmitState
    { postponedUntil :: [PP m] -- location after which we can run cont, cont
    , unevaluatedNodes :: [DgExt]
    , evaluatedSinks :: [DgExt]
    }

getSinks (p :< Sink) = modify (<> pure p)
getSinks (_ :< other) = for_ other getSinks

getNodes (p :< Node w) = modify (<> pure p) *> for_ w getNodes
getNodes (_ :< other) = for_ other getNodes

getNodes' ast = execState (getNodes ast) []


checkDataDep sinks x 
    | (p :< Node w) <- x, elem p sinks = return p 
    | otherwise                        = mempty
-- checkDatatDep _ _ = Nothing

--------------------------------------------------------------------------------

hello ast = execStateT (go ast) (EmitState [] unEvNodes [])

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

    unEvNodes = execState (getNodes ast) []

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
            -> st {postponedUntil=pure (PP p (getNodes' ast1) (go ast1)) <> postponedUntil st}

    runPostponed p = do
        q <- gets postponedUntil
        let qq = filter ((p==).ppPos) q
        modify $ \st
            -> st { postponedUntil = deleteFirstsBy (on (==) ppPos) (postponedUntil st) qq}
--         for_ qq ppCont
        for_ qq $ \pp -> do
            liftIO $ print ("COMPLETING >>")
            ppCont pp
            liftIO $ print ("<< COMPLETED")

    sinks = execState (getSinks ast) []
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

cheers
    :: Monad m
    => (DgExt -> Diagram continuation device label () -> m a)
    -> Cofree (Diagram continuation device label) DgExt
    -> m (EmitState m)
cheers emit ast = execStateT (go ast) (EmitState [] unEvNodes [])

    where

    go x@(p :< Node w) = do
        dataDeps <- getDataDeps w
        locationDeps <- getUnevaluatedAndNotPostponedNodesAt p
        case dataDeps <> locationDeps of
            [] -> do
                markNodeAsEvaluated p
                emit' x
                for_ w go
                runPostponed p
            deps -> do
                let r = maximum deps
                postpone x r
    go x@(p :< Sink) = do
        markSinkAsEvaluated p
        emit' x
        runPostponed p
    go othr@(_ :< x) = emit' othr *> for_ x go

    emit' (p :< x) = lift $ emit p $ fmap (const ()) x

--look for unevaluted nodes on which node depends
    getDataDeps w = do
        let deps = foldMap (checkDataDep sinks) w
        evaluated <- gets evaluatedSinks --XXX why evaluatedSinks? why not all sinks?
        return $ deps \\ evaluated

    markSinkAsEvaluated p = modify $ \st -> st {evaluatedSinks=pure p <> evaluatedSinks st}
    markNodeAsEvaluated p = modify $ \st -> st {unevaluatedNodes=delete p (unevaluatedNodes st)}

    unEvNodes = execState (getNodes ast) []

    --hate this hack :(
    getUnevaluatedAndNotPostponedNodes
        = (\\) <$> gets unevaluatedNodes <*> gets (foldMap ppNodes . postponedUntil)

    getUnevaluatedAndNotPostponedNodesAt p
        = filter (< p) <$> getUnevaluatedAndNotPostponedNodes

    postpone ast1 p
        = modify $ \st
            -> st {postponedUntil=pure (PP p (getNodes' ast1) (go ast1)) <> postponedUntil st}

    runPostponed p = do
        q <- gets postponedUntil
        let qq = filter ((p==).ppPos) q
        modify $ \st
            -> st { postponedUntil = deleteFirstsBy (on (==) ppPos) (postponedUntil st) qq}
        for_ qq ppCont

    sinks = execState (getSinks ast) []

emitPrint p  Sink             = print ("sink", p)
emitPrint p (Source a       ) = print ("src", p)
emitPrint p  End              = print ("end", p)
emitPrint p (Device device a) = print ("dev", device, p)
emitPrint p (Jump   _label  ) = print ("jump", p) *> undefined
emitPrint p (Node   w       ) = print ("node", p)
emitPrint p (Cont   _continuation _a) = undefined
emitPrint p (Conn   _continuation) = undefined

-- emitPrint (p :< Sink           ) = print ("sink", p)
-- emitPrint (p :< Source a       ) = print ("src", p)
-- emitPrint (p :< End            ) = print ("end", p)
-- emitPrint (p :< Device device a) = print ("dev", device, p)
-- emitPrint (p :< Jump   _label  ) = print ("jump", p) *> undefined
-- emitPrint (p :< Node   w       ) = print ("node", p)
-- emitPrint (p :< Cont   _continuation _a) = undefined
-- emitPrint (p :< Conn   _continuation) = undefined

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
                        print (here, "--------------------------------------------------")
--                         sinks ast1
                        u <- cheers emitPrint ast1
                        print $ length $ postponedUntil u
                        print $ length $ unevaluatedNodes u
                        print $ length $ evaluatedSinks u
                        return ()
    where
--     deviceThing = wrapDevice3 (pure . I) (pure . A)
    deviceThing = wrapDeviceSimple
