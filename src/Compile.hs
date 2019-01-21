{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, BangPatterns,
  FlexibleContexts, RecursiveDo, TupleSections, GADTs, DeriveFunctor #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Compile where

import Data.IORef
import qualified Data.Map as M
import Control.Monad.State
import Data.Tree as T
import Data.Traversable
import qualified Data.Set as S
import Data.Foldable

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm

import LadderParser
import DiagramParser

--------------------------------------------------------------------------------

data Pg lbl m a where
	Do :: m a -> Pg lbl m a
	Go :: lbl -> Pg lbl m a
	Br :: lbl -> m Bool -> Pg lbl m a

--------------------------------------------------------------------------------

weave1 :: forall lbl m a. (Eq lbl, Monad m) => [(lbl, Pg lbl m a)] -> m a -> m a
weave1 [] end = end
weave1 src end = snd $ head program
	where

	program = f src

	f [(lbl, x)] = [(lbl, h x end)]
	f ((lbl, x) : xs) = let
		xs'@((_, xx) : _) = f xs
		in (lbl,h x xx) : xs'

	h (Do a) next = a >> next
	h (Go lbl) _ = getLbl lbl
	h (Br lbl cond) next = let
		dst = getLbl lbl
		in cond >>= \flag -> if flag then dst else next

	getLbl lbl = maybe (error "label not found") id $ lookup lbl program

--------------------------------------------------------------------------------

data Js a where
	Js :: String -> Js a
-- 	JsC :: String -> Js Bool

tojs :: (Eq lbl, Show lbl) => [(lbl, Pg lbl Js ())] -> String
tojs program = loop $ foldMap (h . snd) program
	where
	h (Do (Js c)) = [c]
	h (Go lbl) = ["target = " ++ show lbl ++ ";", "continue;"]
	h (Br lbl (Js c)) =
		[ "if ( " ++ c ++ " ) {"
		, "  target = " ++ show lbl ++ ";"
		, "  continue;"
		, "}"
		]
	loop stmts = unlines
		["{"
		, "  var target = \"!start\";"
		, "    while (?) {"
		, "      switch (?) {"
		, unlines $ fmap ("        " ++) stmts
		, "      }"
		, "    }"
		, "}"
		]

	getLbl lbl = maybe (error "label not found") id $ lookup lbl program

--------------------------------------------------------------------------------

gatherNodes :: Symbol -> S.Set Pos
gatherNodes = cata' node
-- gatherNodes :: [ (a0, Cofree Symbol_ Pos)] -> S.Set Pos
-- gatherNodes = foldMap (cata' node . snd)
	where
	node (p, LadderParser.Node x) = S.singleton p <> fold x
	node (_, x) = fold x

compile'
	:: M.Map String (IORef Bool, IORef Bool)
	-> [(Maybe String, Symbol)]
	-> IO [(Maybe String, Pg (Maybe String) IO ())]
compile' vars nets =
	fmap concat $ for nets $ \(lbl, net) -> do
		
		let nodes = toList $ gatherNodes net
		nodes' <- M.fromList <$> for nodes (\n -> (n,) <$> newIORef False)

		print (here, lbl, ">>>")
		net' <- xxxx vars nodes' net
		return $ (lbl, Do $ return ()) : fmap (Nothing,) net'

--------------------------------------------------------------------------------

data Pair a b = Pair a b

instance Eq a => Eq (Pair a b) where
	Pair x _ == Pair y _ = x == y

instance Ord a => Ord (Pair a b) where
	Pair x _ <= Pair y _ = x <= y

instance Show a => Show (Pair a b) where
	show (Pair x _) = show x

jjj_ :: Cofree Symbol_ Pos -> AdjacencyMap (Pair Pos (Symbol_ Symbol))
jjj_ (p :< xx@(Source x)) = go (Pair p xx) x
	where
	go parent (p :< y)
		= overlays $ edge parent this : foldMap ((:[]) . go this) y
		where
		this = Pair p y
jjj_ _ = error here --parser should not allow this


-- iii :: Cofree Symbol_ Pos -> M.Map Pos (Symbol_ (Cofree Symbol_ Pos))
-- iii = go
-- 	where
-- 	go (p :< y) = M.singleton p y <> foldMap go y


xxxx
	:: M.Map String (IORef Bool, IORef Bool) --TODO more types
	-> M.Map Pos (IORef Bool) --nodes
	-> Symbol
	-> IO [Pg (Maybe String) IO ()]
xxxx vars nodes net = do

-- 	let vars = M.empty --FIXME
-- 	vars <- M.fromList <$> for
-- 		["%IX0", "%QX0", "%IX1", "%MX0", "%MX1"]
-- 		(\n -> (n,) <$> newIORef False)

	p <- xxxxX vars nodes (dfsForest $ jjj_ net)

-- 	for order $ \(T.Node a forest) -> do
-- -- 		error here
-- 		return ()
	print (here, "------------------------------")
	print $ topSort $ jjj_ net
	return p

#if 1
devices =
	[ dev "[ ]" 1 $ \[a] -> op (&&) a
	, dev "[/]" 1 $ \[a] -> op (\p v -> p && not v) a
	, dev "( )" 1 $ \[a] -> update (\p _ -> Just p) a
	, dev "(S)" 1 $ \[a] -> update (\p _ -> if p then Just True else Nothing) a
	, dev "(R)" 1 $ \[a] -> update (\p _ -> if p then Just False else Nothing) a
	]
	where
	dev n na f = (n, na, f)
	op = undefined
	update = undefined
#endif

xxxxX
	:: M.Map String (IORef Bool, IORef Bool) --TODO more types
	-> M.Map Pos (IORef Bool) --nodes
	-> Forest (Pair Pos (Symbol_ Symbol))
	-> IO [Pg (Maybe String) IO ()]
xxxxX vars nodes nets = do
	nets' <- for nets doNet
	return $ concat nets'

 	where

 	doNet (T.Node (Pair _p Source{}) net') = do
		
		pwr <- newIORef True
		fst <$> runStateT (g pwr net') M.empty
	doNet _ = error here --should not happen

	g :: (IORef Bool)
		-> [Tree (Pair Pos (Symbol_ Symbol))]
		-> StateT (M.Map Pos (IORef Bool)) IO [Pg (Maybe String) IO ()]
	g pwr net = foldM (h pwr) [] net 
-- 	g pwr net = (\a b -> foldM b [] a) net $ \b (n@(T.Node d net')) -> do
-- 		(pwr', pg) <- f pwr d
-- 		((b <> pg) <>) <$> g pwr' net'

-- 	h :: (IORef Bool)
-- 		-> [Tree (Pair Pos (Symbol_ Symbol))]
-- 		-> StateT (M.Map Pos (IORef Bool)) IO [Pg (Maybe String) IO ()]
	h pwr = \b (n@(T.Node d net')) -> do
		(pwr', pg) <- f pwr d
		((b <> pg) <>) <$> g pwr' net'

	f :: IORef Bool
		-> Pair Pos (Symbol_ Symbol)
		-> StateT (M.Map Pos (IORef Bool)) IO (IORef Bool, [Pg (Maybe String) IO ()])
	f pwr (Pair _ (Device body options _)) = do
		args <- for options $ \name -> do
			case M.lookup name vars of
				 Nothing -> fail $ show (here, "variable not found", name)
				 Just v -> return (name, v)
		dev body args
		where
		dev "[ ]" [a] = op (&&) a
		dev "[/]" [a] = op (\p v -> p && not v) a
		dev "( )" [a] = update (\p _ -> Just p) a
		dev "(S)" [a] = update (\p _ -> if p then Just True else Nothing) a
		dev "(R)" [a] = update (\p _ -> if p then Just False else Nothing) a
		dev other _a = error $ show (here, other)

		op f aa@(_, (a, _a')) = liftIO $ newIORef False
			>>= \r -> return (r
				, [Do $ op' f aa r])

		op' f (name, (a, _)) r = do
			va <- readIORef a
			p <- readIORef pwr
			writeIORef r $ f p va

		update f a = return (pwr, [Do $ update' f a])

		update' f (name, (a, a')) = do
			va <- readIORef a
			p <- readIORef pwr
			case f p va of
				 Just v' -> writeIORef a' v'
				 _ -> return ()

	f pwr ((Pair _ (Jump target))) = return (pwr, [Br (Just target) (readIORef pwr)])
	f pwr (Pair p LadderParser.Node{}) = doNode pwr p
--  	f pwr (Pair p Node') = doNode pwr p
	f pwr (Pair _ Sink) = return (pwr, []) -- error $ show (here, "FIXME") --right rail
	f _ (Pair _ Source{}) = error $ show (here, "should not happen")
	f _ (Pair _ LadderParser.Label{}) = error "should not happen"

	doNode pwr p = do
		case M.lookup p nodes of
			Nothing -> error here -- should not happen
			Just nr -> do
				return (nr, [Do $ readIORef pwr >>= \v -> modifyIORef nr (||v) ])
-- 	doNode pwr p = do
-- 		m <- get
-- 		case M.lookup p m of
-- 			Nothing -> do
-- 				nr <- liftIO $ newIORef False
-- 				put $ M.insert p nr m
-- 				return (nr, [Do $ readIORef pwr >>= writeIORef nr ])
-- 			Just nr -> do
-- 				return (nr, [Do $ readIORef pwr >>= \v -> modifyIORef nr (||v) ])

--------------------------------------------------------------------------------
