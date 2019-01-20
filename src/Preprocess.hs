
module Preprocess where

import Data.Char

-- import Text.ParserCombinators.ReadP as RP
import Text.Megaparsec as P hiding (runParser', Pos)
import Text.Megaparsec.Char as PC

--------------------------------------------------------------------------------

--TODO nice leading column
--maybe parse even rungs, its alphabet is not that big
type ParseErr = String
test4 :: Parsec ParseErr String [(Maybe String, [String])]
test4 = ladder <* eof
	where
	ladder = do
		white --FIXME this would eat leading column (when implemented)
		P.many $ (P.many comment) *> eol
		P.some rung

	rung = do
		lbl <- (Just <$> (P.some PC.alphaNumChar <* PC.char ':' <* white <* eol)) <|> pure Nothing
		net <-
			some $
				lookAhead (P.oneOf "|+" P.<|> PC.alphaNumChar)
				*> manyTill anySingle eol
		return (lbl, net)
	comment =
		PC.string "(*" *> manyTill anySingle (try (PC.string "*)"))
		*> white
	white = skipMany (P.satisfy (\c -> isSpace c && c /= '\n')) --FIXME use eol
	eol = PC.char '\n'


preproc2 :: String -> Either String [(Maybe String, [String])]
preproc2 src =
	case parse test4 "(file)" src of
		 Left err -> Left $ show err
		 Right n -> Right n
