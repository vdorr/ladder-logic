
module Language.Ladder.Eval where

import Control.Monad.State

import Language.Ladder.Utils
import Language.Ladder.Types



type EvSt = ()

blargh
    :: (Ord p, Show p, Show l, Monad m)
    => 
--     (d -> m [ExtendedInstruction l (AI (Reg adr) k adr)])
--     -> 
    Cofree (Diagram c d l) p
    -> m ([p]
            , EvSt --AccuEmitState p [(Maybe p, ExtendedInstruction l (AI (Reg adr) k adr))]
            )
blargh ast@(q0 :< _)
    = runStateT
        (traverseDiagram (evalElem) evalPost q0 ast)
        () --(AccuEmitState q0 sinks nodes [] [])
    where
    (nodes, sinks) = collectNodesAndSinks ast

    evalElem = undefined
    evalPost = undefined


    go (Node   []      ) = do
        undefined
    go (Node   w       ) = do
        undefined
    go  Sink             = do
        undefined
    go (Source _a       ) = do
        undefined
    go  End              = do
        undefined
    go (Device device _a) = do
        undefined
    go (Jump   label  ) = do
        undefined
    go (Cont   _continuation _a) = undefined
    go (Conn   _continuation) = undefined
