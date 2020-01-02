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

-- 'postponed' is appended when -see above
data EmitState m = EmitState
    { 
--         postponed :: [(DgExt, m ())] -- original location and continuation
        postponedUntil :: [(DgExt, m ())] -- location after which we can run cont, cont
        , unevaluatedNodes :: [DgExt]
        , evaluatedSinks :: [DgExt]
    }

-- sinks (p :< Sink) = print p
-- sinks (p :< other) = for_ other sinks

getSinks (p :< Sink) = modify (<> pure p)
getSinks (_ :< other) = for_ other getSinks



hello ast = undefined

    where

--     go (p :< Sink) = undefined
    go x@(p :< Node w) = do
        --check if i need to evaluate other line first
        --check data dependency
        let deps :: [DgExt] = foldMap (checkDataDep sinks) w
        q <- gets evaluatedSinks
        let qq = intersect q deps
        case qq of
             [] -> undefined
             deps -> do
                 liftIO $ print $ maximum deps
                 undefined
        rr <- gets unevaluatedNodes
        let u = filter (< p) rr
        case u of
            [] -> undefined -- good, emit
            d -> do -- postpone until `maximum d`
                let r = maximum d
                liftIO $ print r
                undefined
        emit x
    go (_ :< other) = undefined --for_ other go

    emit (p :< Sink) = do
        undefined
    emit _ = undefined

    sinks = execState (getSinks ast) []
    hasDep p = elem p sinks --or "intersectsWithSink"
        --FIXME should be according to (5)
        --FIXME return location

    allLinesAboveEvaluated p = undefined
        --FIXME return location of end of line sink
        -- look for element of unevaluatedNodes strictly northeast of p
        -- or, even better, largest of all lying northeast of p


checkDataDep sinks x 
    | (p :< Node w) <- x, elem p sinks = return p 
    | otherwise                        = mempty
-- checkDatatDep _ _ = Nothing

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
    where
--     deviceThing = wrapDevice3 (pure . I) (pure . A)
    deviceThing = wrapDeviceSimple
