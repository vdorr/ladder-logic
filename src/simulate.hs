{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, BangPatterns,
  FlexibleContexts, OverloadedStrings, TupleSections, GADTSyntax, DeriveFunctor #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import System.Environment (getArgs)
-- import Control.Monad.State
-- import Control.Monad.Except
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

-- import Control.Applicative hiding (many)

import Debug.Trace
import Text.Read (readEither)
-- import qualified Data.Map.Strict as M
import qualified Data.Map as M
-- import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- import Algebra.Graph.AdjacencyMap
-- import Algebra.Graph.AdjacencyMap.Algorithm

import Preprocess
import DiagramParser
import LadderParser
import Compile
import Ladder.Javascript

--------------------------------------------------------------------------------

lpad :: a -> Int -> [a] -> [a]
lpad filler n str = reverse $ take n $ reverse str ++ repeat filler

--------------------------------------------------------------------------------

interpret :: [(Maybe String, Symbol)] -> IO ()
interpret nets = do

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

data Size = X | B | W | D | L deriving (Show, Read) -- 1 8 16 32 64
data Value = X' Bool | W' Int deriving Show
data Section = I | Q | M deriving (Show, Read)

parseVarName :: String -> Either String (Section, Size, Int)
parseVarName ('%' : sect : ty : loc) -- = error here
	= (,,) <$> readEither [sect] <*> readEither [ty] <*> readEither loc
parseVarName _ = Left "wrong format"

--------------------------------------------------------------------------------

compile'' file = do
	print (here, file)
	TIO.readFile file >>= parseLadder

main :: IO ()
main = do
	print here
	getArgs >>= \case
-- 		["run", file] -> compile file >>= runLadder
		[file] -> do
			nets <- compile'' file
			print (here, "----------------------------------------")
			
			generatejs nets
				>>= TIO.putStrLn

			print (here, "----------------------------------------")

			interpret nets

-- 			forM_ nets $ \(lbl, net) -> do
-- 				print (here, lbl, ">>>")
-- 				xxxx net
			print (here, "DONE")
		_ -> error $ show (here, "wrong arguments")
