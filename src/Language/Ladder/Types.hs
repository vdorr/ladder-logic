
module Language.Ladder.Types
    ( Diagram(..)
    , Operand(..)
    , DevType(..)
    , DgExt
--     , mapDgA, mapDg
    , traverseDiagram
    , checkDataDep, collectNodesAndSinks
    )
where

-- import Data.Functor.Identity
import Control.Monad.State
-- import Data.Traversable
import Data.Foldable
import Data.List
import Data.Bifunctor

import Language.Ladder.Utils

--------------------------------------------------------------------------------

-- |Token position and extent
type DgExt = (Int, (Int, Int))

--------------------------------------------------------------------------------

-- |Contact operand, located above or below
data Operand address
    = Var !address   -- ^name of memory location
    | Lit !Int       -- ^integer literal, usually allowed only below contact
    deriving (Show, Eq, Functor)

data DevType t
    = Coil_    !t
    | Contact_ !t
    deriving (Show, Eq, Functor)

--------------------------------------------------------------------------------

-- |Ladder AST type
data Diagram continuation device label a
    = Source !a   -- ^start of power rail
    | Sink       -- ^where wire connects to (implied) right rail or intersect node
    | End        -- ^where vertical left rail ends at the bottom
--     | Stub       -- ^intersection of hline and node
    | Device !device !a
    | Jump   !label
    | Node   ![a] --order matters here
    | Cont   !continuation !a
    | Conn   !continuation
    deriving (Show, Functor, Eq, Foldable, Traversable)

--------------------------------------------------------------------------------

checkDataDep :: (Foldable t, Eq a, Monad m, Monoid (m a)) =>
                t a -> Cofree (Diagram c d l) a -> m a
checkDataDep sinks x 
    | (p :< Node _) <- x, elem p sinks = return p 
    | otherwise                        = mempty


collectNodesAndSinks :: Cofree (Diagram c d l) a -> ([a], [a])
collectNodesAndSinks ast = execState (go ast) ([], [])
    where   go (p :< Node w) = modify (first (p:)) *> for_ w go
            go (p :< Sink  ) = modify (second (p:))
            go (_ :< other ) = for_ other go

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

data PP p m a = PP
    { ppPos   :: p
    , ppNodes :: [p]
    , ppCont  :: StateT (TraverseState p m a) m ()
    }

data TraverseState p m a = TraverseState
    { tsPostponed      :: [PP p m a] -- location after which we can run cont, cont
    , unevaluatedNodes :: [p]
    , evaluatedSinks   :: [p]
    }

traverseDiagram
    :: (Ord p, Monad m)
    => (a -> p -> Diagram c d l (Cofree (Diagram c d l) p) -> m a)
    -> (a -> Cofree (Diagram c d l) p -> m ())
    -> a
    -> Cofree (Diagram c d l) p
    -> m [p] -- (TraverseState p m a)
traverseDiagram emit post q0 ast
    = (fmap ppPos.tsPostponed) <$> execStateT (go q0 ast) (TraverseState [] unEvNodes [])
--     =  execStateT (go q0 ast) (TraverseState [] unEvNodes [])

    where

    go q x@(p :< Node w) = do
        dataDeps     <- getDataDeps w
        locationDeps <- getUnevaluatedAndNotPostponedNodesAt p
        case dataDeps <> locationDeps of
            []   -> markNodeAsEvaluated p *> evalV q x
            deps -> postpone x (maximum deps) q --postpone until most distant dependecy
    go q x@(p :< Sink) = markSinkAsEvaluated p *> evalV q x
    go q other = evalV q other

    evalV q (p :< w) = do
        q' <- lift (emit q p w)
        runPostponed p
        for_ w (go q')

--look for unevaluted nodes on which node depends
--returns not yet evaluated dependencies
    getDataDeps w = do
        let deps = foldMap (checkDataDep sinks) w
        evaluated <- gets evaluatedSinks
        return $ deps \\ evaluated

    markSinkAsEvaluated p = modify \st -> st {evaluatedSinks = p : evaluatedSinks st}
    markNodeAsEvaluated p = modify \st -> st {unevaluatedNodes = delete p (unevaluatedNodes st)}

    --hate this hack :(
    getUnevaluatedAndNotPostponedNodes
        = (\\) <$> gets unevaluatedNodes
               <*> gets (foldMap ppNodes . tsPostponed)

    getUnevaluatedAndNotPostponedNodesAt p
        = filter (< p) <$> getUnevaluatedAndNotPostponedNodes

    postpone x untilPosition q = do
        let stub = PP untilPosition (fst (collectNodesAndSinks x)) (go q x)
        modify \st -> st {tsPostponed = stub : tsPostponed st}
        lift $ post q x

    runPostponed p = do
        (now, later) <- gets (partition ((p==).ppPos) . tsPostponed)
        modify \st -> st { tsPostponed = later }
        for_ now ppCont

    (unEvNodes, sinks) = collectNodesAndSinks ast
