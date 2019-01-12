{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards,
  FlexibleContexts, RecursiveDo, TupleSections, GADTSyntax #-}

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

import Data.Vector (Vector, indexed, fromList, (!?), (//), (!))
-- import Control.Applicative hiding (many)

-- import Debug.Trace
-- import Text.Read (readEither)
-- import qualified Data.Map.Strict as M
import qualified Data.Map as M
-- import qualified Data.Set as S

-- import Text.ParserCombinators.ReadP as RP
import Text.Parsec as P

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm

import DiagramParser
import LadderParser

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

--TODO nice leading column
--maybe parse even rungs, its alphabet is not that big
test4 = ladder <* eof
	where
	ladder = do
		white --FIXME this would eat leading column (when implemented)
		P.many $ (P.many comment) *> eol
		P.many1 rung

	rung = do
		lbl <- optionMaybe (many1 alphaNum <* P.char ':' <* white <* eol)
		net <-
			many1 $
				lookAhead (P.oneOf "|+" P.<|> alphaNum)
				*> manyTill anyChar eol
		return (lbl, net)
	comment =
		P.string "(*" *> manyTill anyChar (try (P.string "*)"))
		*> white
	white = skipMany (P.satisfy (\c -> isSpace c && c /= '\n'))
	eol = P.char '\n'


preproc2 :: String -> Either String [(Maybe String, [String])]
preproc2 src =
	case parse test4 "(file)" src of
		 Left err -> Left $ show err
		 Right n -> Right n

--------------------------------------------------------------------------------

-- data V = V Pos (Symbol_ ())
-- instance Eq V where
-- 	V a _ == V b _ = a == b
-- instance Ord V where
-- 	V a _ <= V b _ = a <= b

-- unFix :: Fix f -> f (Fix f)
-- unFix (Fx x) = x

unFix :: Cofree f a -> (a, f (Cofree f a))
unFix (a :< x) = (a, x)

cata :: Functor f => ((w, f a) -> a) -> Cofree f w -> a
cata alg = alg . fmap (fmap (cata alg)) . unFix

hhh :: Cofree Symbol_ Pos -> AdjacencyMap (Const Pos Symbol_)
hhh (p :< Source x) = undefined -- go p x empty
	where

	go :: AdjacencyMap (Const Pos Symbol_)
		-> Pos
		-> Cofree Symbol_ Pos
		-> AdjacencyMap (Const Pos Symbol_)
	go = undefined

hhh _ = error here

xxxx :: Symbol -> IO ()
xxxx net = undefined
-- 	where
-- 	f (Source pos next) = undefined
-- 	f (Sink pos) = undefined
-- 	f (Device pos (body :: String) (options :: [String])
-- 		(output :: Pos) next ) = undefined
-- 	f (Jump pos target) = undefined
-- 	f Label{} = error "should not happen"
-- 	f (Node pos (succs :: [Symbol])) = undefined



data Pg lbl m a where
	Do :: m a -> Pg lbl m a
	Go :: lbl -> Pg lbl m a
	Br :: lbl -> m Bool -> Pg lbl m a


-- xxxxX
-- 	:: M.Map (Maybe String) (IO ())
-- 	-> M.Map String (IORef Bool) --TODO more types
-- 	-> Symbol
-- 	-> IO [Pg (Maybe String) IO ()]
-- xxxxX rungs vars (Source p next) = do
-- 	pwr <- newIORef True
-- 	fst <$> runStateT (f pwr next) M.empty
-- 
-- 	where
-- 
-- 	f
-- 		:: (IORef Bool)
-- 		-> Symbol
-- 		-> StateT (M.Map Pos (IORef Bool)) IO [Pg (Maybe String) IO ()]
-- 
-- -- 		(body :: String) (options :: [String]) (output :: Pos)
-- 	f (pwr) (Device p body options output next) =
-- 		undefined
-- 
-- 	f pwr (Jump _p target) = return [Br (Just target) (readIORef pwr)]
-- 		--readIORef pwr >>= \c -> if c then join (getRung target) else return ()
-- 
-- -- 		succs :: [Symbol]
-- 	f pwr (Node p succs) = for succs undefined
-- 	f pwr (Node' p) = [] <$ modify (M.insert p pwr)
-- 
-- 	f _ Sink{} = error $ show (here, "FIXME") --right rail
-- 
-- 	f _ Source{} = error $ show (here, "should not happen")
-- 	f _ Label{} = error "should not happen"
-- 
-- 	getRung lbl =
-- 		case M.lookup (Just lbl) rungs of
-- 			 Just r -> return r
-- 			 Nothing -> error $ show (here, lbl, "not found")
-- 
-- xxxxX _ _ _ = error $ show (here, "should not happen")
-- 
-- 
-- xxxxY :: [(Maybe String, Symbol)] -> IO ()
-- xxxxY rungs = mdo
-- 	let vars = M.empty
-- -- 	rungs <- (\f -> foldM f [] rungs) $ \m (lbl, net) -> do
-- 	rungs' <- forM rungs $ \(lbl, net) -> do
-- 		undefined
-- -- 		return (lbl, xxxxX rungs'' vars net)
-- 
-- -- 	let rungs'' = M.fromList rungs'
-- 	return ()
-- -- foldM
-- --   :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b

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

run :: [(Maybe String, [(Pos, Gizmo)])] -> IO ()
run net = run' vars M.empty states net'
	where
	net' = foldMap snd net

 	run' io out m ((p, x):xs) =
		let
			(out', m') = step io m p x
			out'' = M.union out' out
		in run' io out'' m' xs
 	run' io out m [] = do
		let io' = M.union out io
		print (here, io')
		run' io' M.empty m net'

	vars = M.fromList $ zip (gatherVariables net) (repeat $ X' False)
	states = M.fromList $ zip (foldMap (getTempVars) net) (repeat $ X' False)

	getTempVars :: (Maybe String, [(Pos, Gizmo)]) -> [Pos]
	getTempVars (_, l) = foldMap getOut l

	gatherInputs m = X' . or . fmap ((\(X' v) -> v) . (m M.!))
	
	step io m p Source{} = (io, M.adjust (const $ X' True) p m)
	step io m _p Device {options=[v], ..} = (io', M.adjust (const $ X' y) output m)
		where
		X' pwr = gatherInputs m predecessors
		X' var = case M.lookup v io of
			Nothing -> error here
			Just var' -> var'

		op "[ ]" = (M.empty, pwr && var)
		op "[/]" = (M.empty, pwr && not var)
		op "( )" = (setIo (X' pwr), pwr)
		op "(S)" = (if pwr then setIo (X' True) else M.empty, pwr)
		op "(R)" = (if pwr then setIo (X' False) else M.empty, pwr)
		op other = error $ show (here, other)

		(io', y) = op body
		setIo w = M.fromList [(v, w)]

	step _io _m _p Device{} = error here
	step _io _m _p Jump{} = error here
	step _io _m _p Label{} = error here --should not happen
	step io m p Node{..} = (io, M.adjust (const (gatherInputs m predecessors)) p m)
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
			forM_ nets $ \(lbl, net) -> do
				print (here, lbl, ">>>")
				xxxx net
			print (here, "DONE")
		_ -> error $ show (here, "wrong arguments")
