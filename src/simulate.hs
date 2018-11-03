{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-incomplete-patterns
                     -fwarn-unused-binds
                     -fwarn-unused-imports #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")
import System.Environment (getArgs)
import Control.Monad.State
import Control.Monad.Except
import Data.Char
import Data.List
--TODO import Data.Vector.Unboxed
-- import Data.Vector hiding ((++), forM_, modify, sequence_, sequence)
import Data.Vector (Vector, indexed, fromList, (!?), (//), (!))
import Control.Applicative hiding (many)
import Debug.Trace
import Text.Read (readEither)
import qualified Data.Map.Strict as M
-- import qualified Data.Set as S

import DiagramParser
import LadderParser


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

compile :: FilePath -> IO [(Maybe String, [(Pos, Symbol)])]
compile file = do
	print (here, file)
	s <- readFile file
	let lines' = lines s
	let nrW = 1 + length (show (length lines'))
-- 			let maxX = maximum $ fmap length lines'
-- 			let wX = length $ show maxX
	putStrLn ""
	forM_ (zip [0..] lines') $ \(n::Int, l) ->
		putStrLn $ lpad ' ' nrW (show n) ++ ": " ++ l
	putStrLn ""
	putStrLn "---------------------"

	let r = runParser 0 0 s take3 (Pos(0,0), [])
-- 	let r = runRun 0 0 s take2

	case r of
		Left e -> error $ show (here, e)
		Right (ast, PS{..}) -> do

			putStrLn ""
			print (here, "unparsed:", onlySpaceOrCommentsRemain chars)
			putStrLn ""

			print (here, ast)
			return undefined
-- 		Right (_, PS{..}) -> do
-- #if 0
-- 			putStrLn ""
-- 			forM_ (sortOn (\((x, y), _) -> (y, x)) $ snd user)
-- 				$ \((x, y), g) ->
-- 					putStrLn $ lpad ' ' nrW (show y)
-- 						++ ": "  ++ lpad ' ' 4 (show x)
-- 						++ " "  ++ show g
-- 			putStrLn ""
-- 			forM_ bits $ \row -> do
-- 				forM_ row $ \b -> putStr $ if b then "." else "●" --show $ fromEnum b
-- 				putStrLn ""
-- #endif
-- 
-- 			putStrLn ""
-- 			putStrLn "location is printed as col/row"
-- 			putStrLn ""
-- 
-- 			putStrLn $ lpad ' ' 4 "y" ++ lpad ' ' 3 "x"
-- 			putStrLn $ lpad ' ' 4 "line"
-- 			forM_ (omg $ snd user) $ \(lbl, l) -> do
-- 				print lbl
-- 				forM_ l $ \(Pos (x, y), g) -> do
-- -- 							putStrLn $ "  " ++ show g
-- 					putStrLn $ lpad ' ' 4 (show y)
-- 						++ lpad ' ' 3 (show x)
-- 						++ ":  "  ++ show g
-- 
-- 			return $ omg $ snd user

--------------------------------------------------------------------------------

main :: IO ()
main = do
	print here
	getArgs >>= \case
-- 		["run", file] -> compile file >>= runLadder
		[file] -> do
			compile file
			print (here, "DONE")
		_ -> error $ show (here, "wrong arguments")
