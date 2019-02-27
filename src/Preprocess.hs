{-# LANGUAGE CPP, OverloadedStrings #-}

module Preprocess where

import Data.Char

import Text.Megaparsec --as P hiding (runParser', Pos)
import Text.Megaparsec.Char --as PC

import qualified Data.Text as T
import Data.Text (Text)

--------------------------------------------------------------------------------

--FIXME FIXME add some positional info (at least line numbers)

--TODO nice leading column
--maybe parse even rungs
type ParseErr = String

-- test4 :: Parsec ParseErr String [(Maybe String, [String])]
-- test4 = ladder <* eof
-- 	where
-- 	ladder
-- 		= white --FIXME this would eat leading column (when implemented)
-- 		*> many (many comment *> eol)
-- 		*> some rung
-- 	rung = (,) <$> optional label <*> some network
-- 	label = some alphaNumChar <* char ':' <* white <* eol
-- 	network --TODO erase comments
-- 		= lookAhead (oneOf "|+") -- <|> alphaNumChar)
-- 		*> manyTill anySingle eol
-- 	comment = comment' *> white
-- 	comment' = string "(*" *> manyTill anySingle (try (string "*)"))
-- 	white = skipMany (satisfy (\c -> isSpace c && c /= '\n')) --FIXME use eol
-- 	eol = char '\n' --FIXME use parsec
-- 
-- 
-- preproc2 :: String -> Either String [(Maybe String, [String])]
-- preproc2 src =
-- 	case parse test4 "(file)" src of
-- 		 Left err -> Left $ show err
-- 		 Right n -> Right n

--------------------------------------------------------------------------------


-- text s = string (T.pack s)

test5 :: Parsec ParseErr Text [(Maybe String, [String])]
test5 = ladder <* eof
	where
	ladder
		= white --FIXME this would eat leading column (when implemented)
		*> many (many comment *> eol)
		*> some rung
	rung = (,) <$> optional label <*> some network
	label = some alphaNumChar <* char ':' <* white <* eol
--TODO 	label = takeWhile1P Nothing isAlphaNum

	network --TODO erase comments
		= lookAhead (oneOf [ '|', '+' ]) -- <|> alphaNumChar)
		*> manyTill anySingle eol
	comment = comment' *> white
	comment' = chunk "(*" *> manyTill anySingle (try (chunk "*)"))
	white = skipMany (satisfy (\c -> isSpace c && c /= '\n')) --FIXME use eol

-- 	eol = char '\n' --FIXME use parsec


preproc3 :: T.Text -> Either Text [(Maybe String, [String])]
preproc3 src =
	case parse test5 "(file)" src of
		 Left err -> Left $ T.pack $ show err
		 Right n -> Right n

--------------------------------------------------------------------------------

data Tok
	= Node
	| VLine

	| Label T.Text -- "LABEL:"

	| HLine -- Int --repetitions
	| REdge -- as block input "--->"
	| FEdge -- as block input "---<"
	| Negated -- on block i/o "---0|" or "|0---"
	| Contact T.Text -- "---[OP]---"
	| Coil T.Text -- "---(OP)---"
	| Jump' T.Text -- "--->>LABEL"
	| Connector T.Text -- "--->NAME>"
	| Continuation T.Text -- ">NAME>---"
	| Return -- "---<RETURN>"
	| Store T.Text -- FBD only "---VARIABLE"

test6 :: Parsec ParseErr Text [ (SourcePos, [((SourcePos, SourcePos), Tok)]) ]
test6 = many ln <* eof
	where
	ln
		= white
		*> many (comments *> eol)
		*> ((,) <$> getSourcePos <*> tokens)
-- 		<* eol
	--ehm "not cheap"
-- 	tok = (\a b c -> ((a, c), b)) <$> getSourcePos <*> tok' <*> getSourcePos
	tok t p = ((,t)) <$> ((,) <$> getSourcePos <*> (p *> getSourcePos))
	tokens = undefined
	ln'
		=   tok Node (char '+')
		<|> tok VLine (char '|')
		<|> (tok Label (some alphaNumChar <* char ':') <* comments <* eol)
		<|> hline

	hline
		=   HLine <$ many (char '-')
		<|> REdge <$ char '>'
		<|> FEdge <$ char '<'
		<|> Negated <$ char '0'
-- 	tok'
-- 		=   Node <$ char '+'
-- 		<|> VLine <$ char '|'
-- 		<|> HLine <$ many (char '-')
-- 		<|> REdge <$ char '>'
-- 		<|> FEdge <$ char '<'
-- 		<|> Negated <$ char '0'
#if 0
	rung = (,) <$> optional label <*> some network
	label = some alphaNumChar <* char ':' <* white <* eol
--TODO 	label = takeWhile1P Nothing isAlphaNum
#endif
	comments = many $ comment' *> white --should be called whitespace
	comment' = chunk "(*" *> manyTill anySingle (try (chunk "*)"))
	spaceButNotEOL = satisfy (\c -> isSpace c && c /= '\n')
	white = skipMany spaceButNotEOL
	somewhite = () <$ some spaceButNotEOL
	



-- getSourcePos :: MonadParsec e s m => m SourcePos 
-- SourcePos name line col

--now with columns stored i can eat tokens almost randomly

--TODO should also work for FBD
preproc4 :: Text -> Either Text [ (SourcePos, [((SourcePos, SourcePos), Tok)]) ]
preproc4 src =
	case parse test6 "(file)" src of
		 Left err -> Left $ T.pack $ show err
		 Right n -> Right n

--------------------------------------------------------------------------------
