{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, BangPatterns,
  FlexibleContexts, RecursiveDo, TupleSections, GADTSyntax, DeriveFunctor #-}

{-# OPTIONS_GHC -fno-warn-tabs -fwarn-incomplete-patterns
                     -fwarn-unused-binds
                     -fwarn-unused-imports #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")
import System.Environment (getArgs)
import Control.Monad.State
import Control.Monad.Except
-- import Data.Maybe
import Data.Traversable
import Data.Char
import Data.IORef
import Data.Functor.Const
-- import Data.List
--TODO import Data.Vector.Unboxed
-- import Data.Vector hiding ((++), forM_, modify, sequence_, sequence)
import Control.Concurrent

import Data.Vector (Vector, indexed, fromList, (!?), (//), (!))
-- import Control.Applicative hiding (many)

import Debug.Trace
-- import Text.Read (readEither)
-- import qualified Data.Map.Strict as M
import qualified Data.Map as M
-- import qualified Data.Set as S

import Data.Tree as T
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm

import Preprocess
import DiagramParser
import LadderParser
import Compile

--------------------------------------------------------------------------------

onlySpaceOrCommentsRemain :: Vector (Vector (Bool, Char)) -> Either String ()
onlySpaceOrCommentsRemain c = 
	do
		forM_ (indexed c) $ \(y, row) -> do
--TODO comments
			forM_ (indexed row) $ \(x, (visited, c')) -> do
				unless (visited || isSpace c') $
					throwError $ show (here, (x, y), c')

--------------------------------------------------------------------------------

lpad :: a -> Int -> [a] -> [a]
lpad filler n str = reverse $ take n $ reverse str ++ repeat filler

--------------------------------------------------------------------------------

interpret :: [(Maybe String, Symbol)] -> IO ()
interpret nets = do
	vars <- M.fromList <$> for
-- 		["%IX0", "%QX0", "%IX1", "%MX0", "%MX1"]
		["%MX0", "%MX1"]
		(\n -> (n,) <$> ((,) <$> newIORef False <*> newIORef False))
	nets' <- for nets $ \(lbl, net) -> do
		print (here, lbl, ">>>")
		net' <- xxxx vars net
		return $ (lbl, Do $ return ()) : fmap (Nothing,) net'
#if O
	let !prog = weave1 (concat nets') (return ())

	forever $ do
		threadDelay 50000
		print "------------------"
-- 		forM_ (M.toList vars) $ \(n, (r, r')) -> do
-- 			v <- readIORef r
-- 			print (here, "b", n, v)
		prog
-- 		forM_ (M.toList vars) $ \(n, r) -> readIORef r >>= \v -> print (here, n, v)
		forM_ (M.toList vars) $ \(n, (r, r')) -> do
			v <- readIORef r'
			writeIORef r v
			print (here, "a", n, v)
#else
	let !prog = weave1
		((Just "!!!", Do $ threadDelay 50000>>print "------------------") : concat nets' ++
			[ (Nothing, Do $ forM_ (M.toList vars) $ \(n, (r, r')) -> do
				v <- readIORef r'
				writeIORef r v
				print (here, "a", n, v))
			, (Nothing, Go $ Just "!!!")]
			)
		(return ())

	prog


#endif
	return ()

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


xxxx :: M.Map String (IORef Bool, IORef Bool) --TODO more types
	-> Symbol -> IO [Pg (Maybe String) IO ()]
xxxx vars net = do

-- 	let vars = M.empty --FIXME
-- 	vars <- M.fromList <$> for
-- 		["%IX0", "%QX0", "%IX1", "%MX0", "%MX1"]
-- 		(\n -> (n,) <$> newIORef False)
	p <- xxxxX vars (dfsForest $ jjj_ net)

-- 	for order $ \(T.Node a forest) -> do
-- -- 		error here
-- 		return ()
	print (here, "------------------------------")
	print $ topSort $ jjj_ net
	return p


xxxxX
	:: M.Map String (IORef Bool, IORef Bool) --TODO more types
	-> Forest (Pair Pos (Symbol_ Symbol))
	-> IO [Pg (Maybe String) IO ()]
xxxxX vars nets = do --(Source p next) = do
	nets' <- for nets $ \case
		T.Node (Pair _p Source{}) net' -> do
			pwr <- newIORef True
			fst <$> runStateT (g pwr net') M.empty
		_ -> error here --should not happen
	return $ concat nets'

 	where

	g :: (IORef Bool)
		-> [Tree (Pair Pos (Symbol_ Symbol))]
		-> StateT (M.Map Pos (IORef Bool)) IO [Pg (Maybe String) IO ()]
	g pwr net = (\a b -> foldM b [] a) net $ \b (n@(T.Node d net')) -> do
		(pwr', pg) <- f pwr d
		((b <> pg) <>) <$> g pwr' net'

	f :: (IORef Bool)
		-> Pair Pos (Symbol_ Symbol)
		-> StateT (M.Map Pos (IORef Bool)) IO (IORef Bool, [Pg (Maybe String) IO ()])
	f pwr (Pair _ (Device body options _)) = do
		args <- for options $ \name -> do
			case M.lookup name vars of
				 Nothing -> fail here
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
 	f pwr (Pair p Node') = doNode pwr p
	f pwr (Pair _ Sink) = return (pwr, []) -- error $ show (here, "FIXME") --right rail
	f _ (Pair _ Source{}) = error $ show (here, "should not happen")
	f _ (Pair _ LadderParser.Label{}) = error "should not happen"

	doNode pwr p = do
		m <- get
		case M.lookup p m of
			Nothing -> do
				nr <- liftIO $ newIORef False
				put $ M.insert p nr m
				return (nr, [Do $ readIORef pwr >>= writeIORef nr ])
			Just nr -> do
				return (nr, [Do $ readIORef pwr >>= \v -> modifyIORef nr (||v) ])

--------------------------------------------------------------------------------

parseNet :: [String] -> Either String Symbol --(b, PS st Char)
parseNet net = do
	(ast, PS{..}) <- runParser' 0 0 net take3 (Pos(0,0), [])
	onlySpaceOrCommentsRemain chars
	return ast

--------------------------------------------------------------------------------

#if 0
gatherVariables :: [(Maybe String, [(Pos, Gizmo)])] -> [String]
gatherVariables = nub . foldMap (foldMap (variables . snd) . snd)
	where
	variables Device { options = vars } = vars
	variables _ = []

data Size = X | B | W | D | L deriving (Show, Read) -- 1 8 16 32 64
data Value = X' Bool | W' Int deriving Show
data Section = I | Q | M deriving (Show, Read)

parseVarName :: String -> Either String (Section, Size, Int)
parseVarName ('%' : sect : ty : loc) -- = error here
	= (,,) <$> readEither [sect] <*> readEither [ty] <*> readEither loc
parseVarName _ = Left "wrong format"
#endif

--------------------------------------------------------------------------------

compile :: FilePath -> IO [(Maybe String, Symbol)]
compile file = do		 --print (here, start)

	print (here, file)
	s <- readFile file

	nets <- case preproc2 s of
		Left err -> error $ show (here, err) --TODO fail properly
		Right l -> return l

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
				print (here, ast)
				return (lbl, ast)

--------------------------------------------------------------------------------

main :: IO ()
main = do
	print here
	getArgs >>= \case
-- 		["run", file] -> compile file >>= runLadder
		[file] -> do
			nets <- compile file
			print (here, "----------------------------------------")
			interpret nets
-- 			forM_ nets $ \(lbl, net) -> do
-- 				print (here, lbl, ">>>")
-- 				xxxx net
			print (here, "DONE")
		_ -> error $ show (here, "wrong arguments")
