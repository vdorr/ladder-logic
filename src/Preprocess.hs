
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
		= lookAhead (oneOf ("|+")) -- <|> alphaNumChar)
		*> manyTill anySingle eol
	comment = comment' *> white
	comment' = chunk (T.pack "(*") *> manyTill anySingle (try (chunk (T.pack "*)")))
	white = skipMany (satisfy (\c -> isSpace c && c /= '\n')) --FIXME use eol
	eol = char '\n' --FIXME use parsec


preproc3 :: T.Text -> Either Text [(Maybe String, [String])]
preproc3 src =
	case parse test5 "(file)" src of
		 Left err -> Left $ T.pack $ show err
		 Right n -> Right n

--------------------------------------------------------------------------------

data Tok
	= Node
	| VLine
	| HLine -- Int --repetitions
	| REdge -- as block input "--->"
	| FEdge -- as block input "---<"
	| Negated -- on block i/o "---0|" or "|0---"
	| Contact T.Text -- "---[OP]---"
	| Coil T.Text -- "---(OP)---"
	| Label T.Text -- "LABEL:"
	| Jump' T.Text -- "--->>LABEL"
	| Connector T.Text -- "--->NAME>"
	| Continuation T.Text -- ">NAME>---"
-- 	| BlockWall -- not possible, same as crossing
	| Return -- "---<RETURN>"
	| Store T.Text -- FBD only "---VARIABLE"

--TODO should also work for FBD
preproc4 :: T.Text -> Either T.Text [ (Int, [((Int, Int), Tok)]) ]
preproc4 = undefined
-- getSourcePos :: MonadParsec e s m => m SourcePos 
-- SourcePos name line col

--now with columns stored i can eat tokens almost randomly

--------------------------------------------------------------------------------
