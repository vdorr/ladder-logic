#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Analysis where

import Data.Function
-- import Data.Maybe
import Data.Bifunctor
import Data.List
import Data.Void
import Data.Functor.Identity

-- import Control.Monad.Writer
-- import Control.Monad.State

import Language.Ladder.LadderParser
import Language.Ladder.DiagramParser
import Language.Ladder.Utils

--------------------------------------------------------------------------------

-- data N k op pos = N { nPos :: pos, nId :: k, nDeps :: [k], nOp :: op}
--     deriving Show
-- 
-- nSortSpatial :: Ord pos => [N k op pos] -> [N k op pos]
-- nSortSpatial = sortOn nPos
-- 
-- test asts = flip runState 0 $ runWriterT $ do
--     forM_ asts g
--     where
-- --     sinks = foldMap stubs asts
--     g (p :< a) = f a
--         where
--         f (Source   a ) = undefined
--         f  Sink         = N p <$> name <*> undefined <*> pure a >>= tell1
--         f  End          = undefined
--         f (Device _ a ) = undefined
--         f (Jump _     ) = undefined
--         f (Node     as) = undefined
--         f (Conn c     ) = undefined
--         f (Cont c   a ) = undefined
--     name = lift (get <* modify (+1))
--     tell1 = tell . (:[])

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
-- tsort2
--     :: [(DgExt, [DgExt])]
--     -> [Cofree (Diagram DgExt d s) DgExt]
--     -> [Cofree (Diagram DgExt d s) DgExt]
-- tsort2 mergedNodes asts = fmap snd $ f asts'
--     where
--     asts' = fmap (\n -> (dependencies' n, n)) asts
-- 
--     dependencies' n = deps { nodes = nodes'}
--         where
--         deps = dependencies n
--         nodes' = foldMap allNodeLocs (nodes deps)
-- 
--     --assuming p is in list in mergedNodes
--     allNodeLocs p = [fromMaybe p $ lookup p oldToNew]
--     oldToNew = nub $ foldMap (\(k, xs) -> (k, k) : fmap (,k) xs) mergedNodes
-- 
--     f = sttsort (dependsOn `on` fst)

--assuming spatially sorted input (by lines then by columns)
tsort3
    :: [Cofree (Diagram DgExt d s) DgExt]
    -> [Cofree (Diagram DgExt d s) DgExt]
tsort3 asts = fmap snd $ f asts'
    where
    asts' = fmap (\n -> (dependencies n, n)) asts

    f = sttsort (dependsOn `on` fst)

--------------------------------------------------------------------------------

-- a `dependsOn` b means b has to be evaluated before a
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
--     deriving (Show)

instance Semigroup (Deps p) where
    Deps a b c d <> Deps t u v w = Deps (a <> t) (b <> u) (c <> v) (d <> w)

instance Monoid (Deps p) where
    mempty = Deps [] [] [] []

dependencies :: Cofree (Diagram pos d s) pos -> Deps pos
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
        f (Conn _     ) = Deps [] [] [] [p]
        f (Cont _   a ) = Deps [] [] [p] [] <> a

--------------------------------------------------------------------------------

--TODO TEST every list elemen has all nodes on same line, 'sameLine'
--XXX it don't give element spatially sorted, right?
cut1'
    :: [Cofree (Diagram (Void) d s) DgExt]
    -> [Cofree (Diagram DgExt d s) DgExt]
cut1' = foldMap (uncurry (:) . cut1)

--TODO better name
--TODO ensure cut when line number changes
cut1
    :: Cofree (Diagram (Void) d s) DgExt
    -> ( Cofree (Diagram DgExt d s) DgExt
       , [Cofree (Diagram DgExt d s) DgExt])
cut1 (p :< a) = f a
    where

    f (Source   n) = h Source (cut1 n)
    f  Sink        = (p :< Sink, [])
    f  End         = (p :< End, [])
    f (Device d n) = h (Device d) (cut1 n)
    f (Jump s    ) = (p :< Jump s, [])

--     f (Node     a) = (p :< Node [p :< Conn p], x' ++ concat y)
--         where
--         (x, y) = unzip $ fmap cut1 a
--         x' = (fmap (\n@(pp:<_) -> pp :< Cont p n) x)
    f (Node     m) = (p:< Sink, [p :< Node [p :< Conn p]] ++ x' ++ concat y)
        where
        (x, y) = unzip $ fmap cut1 m
        x' = (fmap (\n@(pp:<_) -> pp :< Cont p n) x)

    f (Conn _    ) = error here
    f (Cont _   _) = error here

    h g w = bimap ((p :<) . g) id w

--specialized to 'DgExt' intentionaly
position :: Cofree f DgExt -> DgExt
position (p :< _) = p

--------------------------------------------------------------------------------

--just remove initial Source and tree of Nodes
--quite useless operation, i think
forest
    :: Cofree (Diagram c d s) p
    -> Maybe [Cofree (Diagram c d s) p]
-- forest (p :< Source a) = Just $ fmap ((p :<) . Source) $ fst $ succs' a
forest (_ :< Source a) = Just $ fmap (\n@(p :< _) -> p :< Source n) $ fst $ succs' a
forest _               = Nothing

follow :: (t -> a) -> Diagram c d s t -> Diagram c d s a
follow g = runIdentity . traverse (Identity . g)

repositionSinks
    :: Eq p
    => [(p, [p])]
    -> Cofree (Diagram c d s) p
    -> Cofree (Diagram c d s) p
repositionSinks nodes = go
    where
    oldToNew = nub $ foldMap (\(k, xs) -> (k, k) : fmap (,k) xs) nodes
    go (p :< Sink) = case lookup p oldToNew of
                          Just p' -> p' :< Sink
                          Nothing -> p :< Sink
    go (p :< other) = p :< follow go other

merge'
    :: Cofree (Diagram c d s) p -- ^input tree
    -> ([(p, [p])], Cofree (Diagram c d s) p)
-- ^pairs of position of 'Node' still in tree and 'Node' positions merged into it and new tree
merge' = mapAccumL (\ns (nss, p) -> (f ns p nss, p)) [] . merge
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
