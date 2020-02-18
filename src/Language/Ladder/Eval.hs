
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
        (traverseDiagram (evalDev) evalPost q0 ast)
        () --(AccuEmitState q0 sinks nodes [] [])
    where
    (nodes, sinks) = collectNodesAndSinks ast

    evalDev = undefined
    evalPost = undefined


