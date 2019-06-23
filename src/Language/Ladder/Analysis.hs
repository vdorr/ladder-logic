#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Analysis where

import Data.Function
import Data.Maybe
import Data.Bifunctor
import Data.Semigroup
import Data.List

import Control.Monad.Writer
import Control.Monad.State


import Language.Ladder.LadderParser
import Language.Ladder.DiagramParser hiding (get, put, modify)
import Language.Ladder.Utils

--------------------------------------------------------------------------------

data N k op pos = N { nPos :: pos, nId :: k, nDeps :: [k], nOp :: op}
    deriving Show

nSortSpatial :: Ord pos => [N k op pos] -> [N k op pos]
nSortSpatial = sortOn nPos

test asts = flip runState 0 $ runWriterT $ do
    forM_ asts g
    where
--     sinks = foldMap stubs asts
    g (p :< a) = f a
        where
        f (Source   a ) = undefined
        f  Sink         = N p <$> name <*> undefined <*> pure a >>= tell1
        f  End          = undefined
        f (Device _ a ) = undefined
        f (Jump _     ) = undefined
        f (Node     as) = undefined
        f (Conn c     ) = undefined
        f (Cont c   a ) = undefined
    name = lift (get <* modify (+1))
    tell1 = tell . (:[])

--------------------------------------------------------------------------------

dropEnd
    :: Cofree (Diagram c d s) p
    -> Cofree (Diagram c d s) p
dropEnd (p :< a) = p :< go a
    where
    go (Node ns) = Node (fmap dropEnd (filter notEnd ns))
    go n         = fmap dropEnd n
    notEnd (_ :< End) = False
    notEnd _          = True

--assuming spatially sorted input (by lines then by columns)
tsort2
    :: [(DgExt, [DgExt])]
    -> [Cofree (Diagram DgExt d s) DgExt]
    -> [Cofree (Diagram DgExt d s) DgExt]
tsort2 mergedNodes asts = fmap snd $ f asts'
    where
    asts' = fmap (\n -> (dependencies' n, n)) asts

    dependencies' n = deps { nodes = nodes'}
        where
        deps = dependencies n
        nodes' = foldMap allNodeLocs (nodes deps)

    --assuming p is in list in mergedNodes
    allNodeLocs p = fromMaybe [p] $ lookup p mergedNodes

    f = sttsort (dependsOn `on` fst)

--------------------------------------------------------------------------------

dependsOn :: Deps DgExt -> Deps DgExt -> Bool
a `dependsOn` b = sinks b `someIsIn` nodes a
                    || conns b `someIsIn` conts a
    where
    someIsIn x y = any (flip elem y) x

-- comesBefore :: Deps DgExt -> Deps DgExt -> Bool
-- b `comesBefore` a = sinks b `someIsIn` nodes a
--                     || conns b `someIsIn` conts a
--     where
--     someIsIn x y = any (flip elem y) x

data Deps p = Deps { nodes, sinks, conts, conns :: [p] }
    deriving (Show)

instance Semigroup (Deps p) where
    Deps a b c d <> Deps t u v w = Deps (a <> t) (b <> u) (c <> v) (d <> w)

instance Monoid (Deps p) where
    mempty = Deps [] [] [] []

dependencies :: Cofree (Diagram DgExt d s) DgExt -> Deps DgExt
dependencies = cata' go
    where
    go (p, n) = f n
        where
        f (Source   a ) = a
        f  Sink         = Deps [] [p] [] []
        f  End          = mempty
        f (Device _ a ) = a
        f (Jump _     ) = mempty
        f (Node     as) = Deps [p] [] [] [] <> mconcat as
        f (Conn c     ) = Deps [] [] [] [p]
        f (Cont c   a ) = Deps [] [] [p] [] <> a

--------------------------------------------------------------------------------

--TODO TEST every list elemen has all nodes on same line, 'sameLine'
--XXX it don't give element spatially sorted, right?
cut1'
    :: [Cofree (Diagram () d s) DgExt]
    -> [Cofree (Diagram DgExt d s) DgExt]
cut1' = foldMap (uncurry (:) . cut1)

sameLine :: Cofree (Diagram c d s) DgExt -> Bool
sameLine n@((ln, _) :< _) = getAll $ foldMap (All.(ln==).fst) n

--TODO better name
--TODO ensure cut when line number changes
cut1
    :: Cofree (Diagram () d s) DgExt
    -> ( Cofree (Diagram DgExt d s) DgExt
       , [Cofree (Diagram DgExt d s) DgExt])
cut1 (p :< a) = f a
    where

    f (Source   a) = h Source (cut1 a)
    f  Sink        = (p :< Sink, [])
    f  End         = (p :< End, [])
    f (Device d a) = h (Device d) (cut1 a)
    f (Jump s    ) = (p :< Jump s, [])

    f (Node     a) = (p :< Node [p :< Conn p], x' ++ concat y)
        where
        (x, y) = unzip $ fmap cut1 a
        x' = (fmap (\n@(pp:<_) -> pp :< Cont p n) x)

    f (Conn _    ) = error here
    f (Cont _   _) = error here

    h g w = bimap ((p :<) . g) id w

--specialized to 'DgExt' intentionaly
position :: Cofree f DgExt -> DgExt
position (p :< _) = p

--------------------------------------------------------------------------------

-- |Return list of annotations (usually positions) of 'Sink' nodes
stubs
    :: Cofree (Diagram c d s) p
    -> [p]
stubs (p :< a) = f a
    where
    f (Source a)   = stubs a
    f  Sink        = [p]
    f  End         = []
    f (Device d a) = stubs a
    f (Jump s)     = []
    f (Node a)     = foldMap stubs a
    f (Conn c    ) = [] --XXX i am not sure !!!
    f (Cont c   a) = []

--just remove initial Source and tree of Nodes
--quite useless operation, i think
forest
    :: Cofree (Diagram c d s) p
    -> Maybe [Cofree (Diagram c d s) p]
-- forest (p :< Source a) = Just $ fmap ((p :<) . Source) $ fst $ succs' a
forest (_ :< Source a) = Just $ fmap (\n@(p :< _) -> p :< Source n) $ fst $ succs' a
forest _               = Nothing

merge'
    :: Cofree (Diagram c d s) p -- ^input tree
    -> ([(p, [p])], Cofree (Diagram c d s) p)
-- ^pairs of position of 'Node' still in tree and 'Node' positions merged into it and new tree
merge' = mapAccumL (\ns (nss, p) -> (f ns p nss, p)) [] . merge
-- merge' ast = mapAccumL (\ns (nss, p) -> (ns ++ nss, p)) [] $ merge ast
    where
    f ns _ []  = ns
    f ns p nss = (p, nss) : ns

merge
    :: Cofree (Diagram c d s) p
    -> Cofree (Diagram c d s) ([p], p)
merge = g
    where
    g (p :< Node as) = (ns, p) :< Node (fmap merge as')
        where
        (as', ns) = foldMap succs' as
    g (p :< other)   = ([], p) :< fmap merge other

-- succs
--     :: Cofree (Diagram d s) p
--     -> [Cofree (Diagram d s) p]
-- succs (_ :< Node xs) = foldMap succs xs
-- succs other          = [other]

succs'
    :: Cofree (Diagram c d s) p
    -> ([Cofree (Diagram c d s) p], [p])
succs' (p :< Node xs) = fmap (++[p]) $ foldMap succs' xs
-- succs' (p :< Node xs) = foldMap succs' xs
succs' other          = ([other], [])

--------------------------------------------------------------------------------
