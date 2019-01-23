{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, BangPatterns,
  FlexibleContexts, MultiParamTypeClasses, TupleSections, GADTs, DeriveFunctor #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Compile where

import Data.IORef
import qualified Data.Map as M
-- import Control.Monad.State
import Control.Monad
import Data.Tree as TR
import Data.Traversable
import qualified Data.Set as S
import Data.Foldable
import Data.List
import qualified Data.Text as T
import Data.Vector (Vector, indexed, fromList, (!?), (//), (!))
import Control.Monad.Except
import Data.Char

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm

import Preprocess
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
	f _ = error here --should not happen

	h (Do a) next = a >> next
	h (Go lbl) _ = getLbl lbl
	h (Br lbl cond) next = let
		dst = getLbl lbl
		in cond >>= \flag -> if flag then dst else next

	getLbl lbl = maybe (error "label not found") id $ lookup lbl program

--------------------------------------------------------------------------------

-- tojs :: (Eq lbl, Show lbl) => [(lbl, Pg lbl Js ())] -> String
tojs :: Eq lbl => [(lbl, [Pg lbl JS ()])] -> String
tojs program = loop $ foldMap f program
	where
	f (lbl, a) = ("case " ++ show (getLbl lbl) ++ ":") : foldMap (fmap ("  " ++) . h) a
	h (Do (JS c)) = [c ++ ";"]
	h (Go lbl) = ["target = " ++ show (getLbl lbl) ++ ";", "continue;"]
	h (Br lbl (JS c)) =
		[ "if ( " ++ c ++ " ) {"
		, "  target = " ++ show (getLbl lbl) ++ ";"
		, "  continue;"
		, "}"
		]
	loop stmts = unlines
		["{"
		, "  var target = 0;"
		, "  while ( env.run() ) {"
		, "    env.scan_begin();"
		, "    switch ( target ) {"
		, unlines $ fmap ("      " ++) stmts
		, "    }"
		, "    env.scan_end();"
		, "  }"
		, "}"
		]
	getLbl lbl = maybe (error "label not found") id $ findIndex ((lbl==).fst) program

--------------------------------------------------------------------------------

gatherNodes :: Symbol -> S.Set Pos
gatherNodes = cata' node
-- gatherNodes :: [ (a0, Cofree Symbol_ Pos)] -> S.Set Pos
-- gatherNodes = foldMap (cata' node . snd)
	where
	node (p, LadderParser.Node x) = S.singleton p <> fold x
	node (_, x) = fold x

gatherVariables :: [(Maybe String, Symbol)] -> S.Set String
gatherVariables = foldMap (cata variables . snd)
	where
	variables x@(Device _ v _) = S.fromList v <> (fold x)
	variables x = fold x
--  S.singleton p <> fold x

--------------------------------------------------------------------------------

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

-- 	let forest = dfsForest $ jjj_ net
	ff <- forM (dfsForest $ jjj_ net) $ \f -> (,f) <$> newIORef True

-- 	p <- xxxxX vars nodes (dfsForest $ jjj_ net)
	p <- xxxxXxx devices vars nodes ff

-- 	for order $ \(T.Node a forest) -> do
-- -- 		error here
-- 		return ()
	print (here, "------------------------------")
	print $ topSort $ jjj_ net
	return p

generatejs :: [(Maybe String, Symbol)] -> IO String
generatejs nets = do

	print (here, toList $ gatherVariables nets)
	
	let vars =
		fmap (\v -> (v, (JSRef $ "\"in " ++ v ++ "\"", JSRef $ "\"out " ++ v ++ "\"")))
		$ toList $ gatherVariables nets
	print (here, vars)

	let nodes =
		fmap (\p -> (p, JSRef $ "\"node " ++ show p ++ "\""))
		$ toList
		$ foldMap (gatherNodes . snd) nets

-- 	let ff = fmap (\net -> (JSRef "pwr -- FIXME", net)) $ dfsForest $ jjj_ nets
	q <- forM nets $ \(lbl, net) -> do
		let ff = fmap (\net -> (JSRef "\"pwr\"", net)) $ dfsForest $ jjj_ net

		(lbl,) <$> xxxxXxx jsdevices (M.fromList vars) (M.fromList nodes) ff

	let env = makeJSEnv vars nodes
	print (here)
	return $ env ++ tojs q


makeJSEnv vars nodes = unlines $ prologue ++ e
	where
	prologue = ["env.m[\"pwr\"] = true;"]
	e = fmap ((++";") . f)
		(let (a, b) = unzip (fmap snd vars) in (a++b) ++ fmap snd nodes)
	f (JSRef v) = "env.m[" ++ v ++ "] = false"

testillytest = q M.empty M.empty []
	where
	q
		:: 
		M.Map String (JSRef Bool, JSRef Bool) --TODO more types
		-> M.Map Pos (JSRef Bool) --nodes
		-> [(JSRef Bool, Tree (Pair Pos (Symbol_ Symbol)))]
		-> IO [Pg (Maybe String) JS ()]
	q = xxxxXxx jsdevices

	
jsget (JSRef r) = "env.get("++ r ++ ")"
jsset (JSRef r) v = "env.set("++ r ++ ", " ++ v ++ ")"
jsif pwr a
	= "if ( " ++ jsget pwr ++ " ) { "
	++ a --"env.set(" ++ a' ++ ", " ++ f pwr ++ "); "
	++ "}"

{-
class Environment {
	constructor() {
		this.m = {};
	}
	run() {
		return true;
	}
	get(name) {
		console.log("1");
	}
	set(name, value) {
		console.log("2");
	}
	scan_begin() {
//TODO copy values from ins to outs
		console.log("3");
	}
	scan_end() {
//TODO copy values from outs to ins
		console.log("4");
	}
}

env = new Environment();
-}

jsdevices :: [(String,
                        (Int,
                         [(a, (JSRef Bool, JSRef Bool))]
                         -> JSRef Bool -> IO (JSRef Bool, [Pg lbl JS ()])))]
jsdevices =
-- 	[ dev "[ ]" 1 $ \[a] -> op "and" a
--  	, dev "[/]" 1 $ \[a] -> op "andn" a
	[ dev "[ ]" 1 $ \[(_, (a, _))] -> update
		(\pwr -> jsset pwr $ jsget pwr ++ " && " ++ jsget a)
 	, dev "[/]" 1 $ \[(_, (a, _))] -> update
		(\pwr -> jsset pwr $ jsget pwr ++ " && !(" ++ jsget a ++ ")")
	, dev "( )" 1 $ \[a] -> update (\pwr -> jsset pwr (jsget pwr))
	, dev "(S)" 1 $ \[(_, (_, a'))] -> update (\pwr -> jsif pwr (jsset a' "true") )
	, dev "(R)" 1 $ \[(_, (_, a'))] -> update (\pwr -> jsif pwr (jsset a' "false") )
	]
	where
	dev n na f = (n, (na, f))

-- 	op f aa@(_, (JSRef a, _a')) pwr'@(JSRef pwr) = return (pwr',
-- -- 		[Do $ JS $ pwr ++ ".set(" ++ f ++ "(" ++ pwr ++ ".get(), " ++ a ++ ".get()))"])
-- 		[Do $ JS $ "env.set(" ++ pwr ++ ", " ++ f ++ "(env.get("++ pwr ++ "), env.get("++ a ++")))"])

-- 	op' pwr f (name, (a, _)) r = undefined

	update f pwr = return (pwr, [Do $ JS $ f pwr])

-- 	update' (pwr) f = JS $ f pwr
-- 		$ "env.set(env.get(" ++ a' ++ "), " ++ f ++ "(" ++ pwr ++ ", env.get(" ++ a ++ ")))"
-- 		$ "if ( " ++ jsget pwr ++ " ) { "
-- 		++ "env.set(" ++ a' ++ ", " ++ f pwr ++ "); "
-- 		++ "}"


#if 1
devices :: [(String,
                        (Int,
                         [(a, (IORef Bool, IORef Bool))]
                         -> IORef Bool -> IO (IORef Bool, [Pg lbl IO ()])))]
devices =
	[ dev "[ ]" 1 $ \[a] -> op (&&) a
	, dev "[/]" 1 $ \[a] -> op (\p v -> p && not v) a
	, dev "( )" 1 $ \[a] -> update (\p _ -> Just p) a
	, dev "(S)" 1 $ \[a] -> update (\p _ -> if p then Just True else Nothing) a
	, dev "(R)" 1 $ \[a] -> update (\p _ -> if p then Just False else Nothing) a
	]
	where
	dev n na f = (n, (na, f))

	op f aa@(_, (a, _a')) pwr = newIORef False
		>>= \r -> return (r
			, [Do $ op' pwr f aa r])

	op' pwr f (name, (a, _)) r = do
		va <- readIORef a
		p <- readIORef pwr
		writeIORef r $ f p va

	update f a pwr = return (pwr, [Do $ update' pwr f a])

	update' pwr f (name, (a, a')) = do
		va <- readIORef a
		p <- readIORef pwr
		case f p va of
				Just v' -> writeIORef a' v'
				_ -> return ()
#endif

doDev devs body args
	= case lookup body devs of
		Just (na, f) | length args == na -> f args
		Just _ -> error $ show (here, "wrong number of args", body)
		Nothing -> error $ show (here, "unknown device", body)

class Ref r m where
	readRef :: r a -> m a
-- 	newRef :: a -> m (r a)
-- 	modifyRef :: r a -> (a -> a) -> m ()
	joinWires :: r Bool -> r Bool -> m ()

instance Ref IORef IO where
	readRef = readIORef
--  	newRef = newIORef
--  	modifyRef = modifyIORef
	joinWires pwr nr = readIORef pwr >>= \v -> modifyIORef nr (||v)

data JSRef a = JSRef String
	deriving Show
data JS a = JS String --(JSRef a)
	deriving Show

instance Ref JSRef JS where
	readRef (JSRef r) = JS $ "env.get(" ++ r ++ ")"
	joinWires (JSRef pwr) (JSRef nr) = JS
		$ "env.set(" ++ nr ++ ", " ++ "env.get(" ++ pwr ++ ") || env.get(" ++ nr ++ "))"

-- xxxxXxx
-- 	:: 
-- -- 	IO (IO ((Bool -> Bool) -> IO Bool)) -> 
-- 	M.Map String (IORef Bool, IORef Bool) --TODO more types
-- 	-> M.Map Pos (IORef Bool) --nodes
-- 	-> [(IORef Bool, Tree (Pair Pos (Symbol_ Symbol)))]
-- 	-> IO [Pg (Maybe String) IO ()]
-- xxxxXxx :: _
xxxxXxx devs vars nodes nets = do
	nets' <- for nets doNet
	return $ concat nets'

 	where

 	doNet (pwr, TR.Node (Pair _p Source{}) net') = g pwr net'
	doNet _ = error here --should not happen

	g pwr net = foldM (h pwr) [] net 

	h pwr = \b (n@(TR.Node d net')) -> do
		(pwr', pg) <- f pwr d
		((b <> pg) <>) <$> g pwr' net'

	f pwr (Pair _ (Device body options _)) = do
		args <- for options $ \name -> do
			case M.lookup name vars of
				 Nothing -> error $ show (here, "variable not found", name)
				 Just v -> return (name, v)
		doDev devs body args pwr

	f pwr ((Pair _ (Jump target))) = return (pwr, [Br (Just target) (readRef pwr)])
	f pwr (Pair p LadderParser.Node{}) = doNode pwr p
	f pwr (Pair _ Sink) = return (pwr, []) -- error $ show (here, "FIXME") --right rail
	f _ (Pair _ Source{}) = error $ show (here, "should not happen")
	f _ (Pair _ LadderParser.Label{}) = error "should not happen"

	doNode pwr p
		= case M.lookup p nodes of
			Nothing -> error here -- should not happen
			Just nr -> return (nr, [Do $ joinWires pwr nr ])

--------------------------------------------------------------------------------

xxxxX
	:: M.Map String (IORef Bool, IORef Bool) --TODO more types
	-> M.Map Pos (IORef Bool) --nodes
	-> Forest (Pair Pos (Symbol_ Symbol))
	-> IO [Pg (Maybe String) IO ()]
xxxxX vars nodes nets = do
	nets' <- for nets doNet
	return $ concat nets'

 	where

 	doNet (TR.Node (Pair _p Source{}) net') = do
		
		pwr <- newIORef True
		g pwr net'
	doNet _ = error here --should not happen

	g :: (IORef Bool)
		-> [Tree (Pair Pos (Symbol_ Symbol))]
		-> IO [Pg (Maybe String) IO ()]
	g pwr net = foldM (h pwr) [] net 

	h pwr = \b (n@(TR.Node d net')) -> do
		(pwr', pg) <- f pwr d
		((b <> pg) <>) <$> g pwr' net'

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

		op f aa@(_, (a, _a')) = newIORef False
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

	doNode pwr p
		= case M.lookup p nodes of
			Nothing -> error here -- should not happen
			Just nr -> return (nr, [Do $ readIORef pwr >>= \v -> modifyIORef nr (||v) ])
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

compile :: T.Text -> IO [(Maybe String, Symbol)]
compile s = do		 --print (here, start)

	nets <- case preproc3 s of
		Left err -> error $ show (here, err) --TODO fail properly
		Right l -> return l

	forM_ nets $ \(lbl, net) -> do
		print ("*****", lbl, "*****")
		forM_ net $ \n -> do
			print n

-- 	let nrW = 1 + length (show (length lines'))
-- 	putStrLn ""
-- 	forM_ (zip [0..] lines') $ \(n::Int, l) ->
-- 		putStrLn $ lpad ' ' nrW (show n) ++ ": " ++ l
-- 	putStrLn ""
-- 	putStrLn "---------------------"

	forM nets $ \(lbl, net) ->
		case parseNet net of
			Left e -> error $ show (here, e)
			Right ast -> do
-- 				print (here, ast)
				return (lbl, ast)

--------------------------------------------------------------------------------

parseNet :: [String] -> Either String Symbol --(b, PS st Char)
parseNet net = do
	(ast, PS{..}) <- runParser' 0 0 net take3 (Pos(0,0), [])
	onlySpaceOrCommentsRemain chars
	return ast

onlySpaceOrCommentsRemain :: Vector (Vector (Bool, Char)) -> Either String ()
onlySpaceOrCommentsRemain c = 
	do
		forM_ (indexed c) $ \(y, row) -> do
--TODO comments
			forM_ (indexed row) $ \(x, (visited, c')) -> do
				unless (visited || isSpace c') $
					throwError $ show (here, (x, y), c')

--------------------------------------------------------------------------------