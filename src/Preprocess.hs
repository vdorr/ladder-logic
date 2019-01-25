
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


preproc3 :: T.Text -> Either String [(Maybe String, [String])]
preproc3 src =
	case parse test5 "(file)" src of
		 Left err -> Left $ show err
		 Right n -> Right n
