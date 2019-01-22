{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, BangPatterns,
  FlexibleContexts, RecursiveDo, TupleSections, GADTSyntax, DeriveFunctor #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import System.Environment (getArgs)
import Control.Monad.State
import Control.Monad.Except
-- import Data.Maybe
import Data.Traversable
import Data.Foldable
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
import Text.Read (readEither)
-- import qualified Data.Map.Strict as M
import qualified Data.Map as M
import qualified Data.Set as S

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

	generatejs nets
		>>= putStrLn

	print (here, toList $ gatherVariables nets)

	vars <- M.fromList <$> for
-- 		["%IX0", "%QX0", "%IX1", "%MX0", "%MX1"]
		["%MX0", "%MX1"]
		(\n -> (n,) <$> ((,) <$> newIORef False <*> newIORef False))
-- 	nets' <- fmap concat $ for nets $ \(lbl, net) -> do
-- 		print (here, lbl, ">>>")
-- 		net' <- xxxx vars net
-- 		return $ (lbl, Do $ return ()) : fmap (Nothing,) net'

	nets' <- compile' vars nets

#if O
	let !prog = weave1 (nets') (return ())
--FIXME init outputs at beginning of scan
	forever $ do
		threadDelay 50000
		print "------------------"
		prog
		forM_ (M.toList vars) $ \(n, (r, r')) -> do
			v <- readIORef r'
			writeIORef r v
			print (here, "a", n, v)
#else
--FIXME init outputs at beginning of scan
	let !prog = weave1
		((Just "!!!", Do $ threadDelay 50000>>print "------------------") : nets' ++
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

--------------------------------------------------------------------------------

parseNet :: [String] -> Either String Symbol --(b, PS st Char)
parseNet net = do
	(ast, PS{..}) <- runParser' 0 0 net take3 (Pos(0,0), [])
	onlySpaceOrCommentsRemain chars
	return ast

--------------------------------------------------------------------------------

data Size = X | B | W | D | L deriving (Show, Read) -- 1 8 16 32 64
data Value = X' Bool | W' Int deriving Show
data Section = I | Q | M deriving (Show, Read)

parseVarName :: String -> Either String (Section, Size, Int)
parseVarName ('%' : sect : ty : loc) -- = error here
	= (,,) <$> readEither [sect] <*> readEither [ty] <*> readEither loc
parseVarName _ = Left "wrong format"

--------------------------------------------------------------------------------

compile :: FilePath -> IO [(Maybe String, Symbol)]
compile file = do		 --print (here, start)

	print (here, file)
	s <- readFile file

	nets <- case preproc2 s of
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
