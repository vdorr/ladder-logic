{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-incomplete-patterns
                     -fwarn-unused-binds
                     -fwarn-unused-imports #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")
import System.Environment (getArgs)
import Control.Monad.State
import Control.Monad.Except
-- import Data.Maybe
import Data.Char
-- import Data.List
--TODO import Data.Vector.Unboxed
-- import Data.Vector hiding ((++), forM_, modify, sequence_, sequence)

import Data.Vector (Vector, indexed, fromList, (!?), (//), (!))
-- import Control.Applicative hiding (many)

-- import Debug.Trace
-- import Text.Read (readEither)
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S

-- import Text.ParserCombinators.ReadP as RP
import Text.Parsec as P

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

preproc :: String -> Either String [String]
preproc src =
	case parse test3 "(file)" src of
		 Left err -> Left $ show err
		 Right n -> Right $ concat n

	where

	test3 = test3' <* eof

	test3' = (:[]) <$> do
		white --FIXME this would eat leading column (when implemented)
		P.many $ (P.many comment) *> eol
		P.many1 rung
		where
		rung =
			lookAhead (P.oneOf "|+" P.<|> alphaNum)
			*> manyTill anyChar eol
		comment =
			P.string "(*" *> manyTill anyChar (try (P.string "*)"))
			*> white
		white = skipMany (P.satisfy (\c -> isSpace c && c /= '\n'))
		eol = P.char '\n'

--------------------------------------------------------------------------------

test4 = test4' <* eof

--TODO nice leading column
--maybe parse even rungs, its alphabet is not that big
test4' = do
	white --FIXME this would eat leading column (when implemented)
	P.many $ (P.many comment) *> eol
	P.many1 rung
	where
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

-- xxxx :: Symbol -> (Maybe String, [Symbol])
-- xxxx = undefined
-- 	where
-- 	f (Source pos (succs :: [Symbol])) = undefined
-- 	f (Sink pos) = undefined
-- 	f (Device pos (body :: String) (options :: [String])
-- 		(output :: Pos) (succs :: [Symbol]) ) = undefined
-- 	f (Jump pos target) = undefined
-- 	f (Label pos (labelName :: String) (nextRung :: Symbol) ) = undefined
-- 	f (Node pos (succs :: [Symbol])) = undefined

--------------------------------------------------------------------------------

parseNet :: [String] -> Either String Symbol --(b, PS st Char)
parseNet net = do
	(ast, PS{..}) <- runParser' 0 0 net take3 (Pos(0,0), [])
	onlySpaceOrCommentsRemain chars
	return ast

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

	forM nets $ \(lbl, net) -> do
-- 		let n = parseNet net

		case parseNet net of
			Left e -> error $ show (here, e)
			Right ast -> do
				print (here, ast :: Symbol)
				return (lbl, ast)

-- 		case r of
-- 			Left e -> error $ show (here, e)
-- 			Right (ast, PS{..}) -> do
-- 
-- 				putStrLn ""
-- 				print (here, "unparsed:", onlySpaceOrCommentsRemain chars)
-- 				putStrLn ""
-- 
-- 				print (here, ast :: Symbol)
-- 				return (lbl, ast)

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
