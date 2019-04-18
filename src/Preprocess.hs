{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")


module Preprocess where

import Data.Char

import Text.Megaparsec --as P hiding (runParser', Pos)
import Text.Megaparsec.Char --as PC
-- import Text.Megaparsec.Char.Lexer (symbol)
import Text.Megaparsec.Debug
import Data.Bifunctor
import Control.Monad hiding (fail)

import Control.Applicative.Combinators (between)

import qualified Data.Text as T
import Data.Text (Text)

import Debug.Trace

--------------------------------------------------------------------------------

--FIXME FIXME add some positional info (at least line numbers)

--TODO nice leading column
--maybe parse even rungs
type ParseErr = String
instance ShowErrorComponent ParseErr where
	showErrorComponent  = show

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
	rung = (,) <$> optional label' <*> some network
	label' = some alphaNumChar <* char ':' <* white <* eol
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
{-
letter       : 'A'..'Z' | '_'
number       : '0'..'9'

token        : node | vline | hline | label ...

target       : number+ | letter+
node         : '+'
vline        : '|'
hline        : '-'+ 
label        : target ':'
redge        : '>'
fedge        : '<'
negated      : '0' '|'
contact      : '[' ('/' | ' ' | 'P' | 'N' | '<' | '>' | '>=' | '<=' | '<>') ']'
coil         : '(' ('/' | ' ' | 'S' | 'R' | 'P' | 'N') ')'
contact'      : '[' anychar+ ']'
coil'         : '(' anychar+ ')'
connector    : '>' letter+ '>'
continuation : connector
return       : '<RETURN>'
jump         : '>>' target
name         : letter+
location     : '%' ('I' | 'Q' | 'M') number+

linecomment  : '//' anychar* '\n'
blockcomment : '(*' anychar* '*)'

-}

type LexSt = ((Int, Int), String)

newtype Lexer a = Lexer { lexer :: LexSt -> Either String (a, LexSt) }

instance Functor Lexer where
	fmap = ap . return

instance Applicative Lexer where
	pure = return
	(<*>) = ap

instance Monad Lexer where
	return a = Lexer $ \s -> return (a, s)
	a >>= b = Lexer $ \s -> do
		(y, s') <- lexer a s
		lexer (b y) s'

getOne :: Lexer Char
getOne = Lexer $ \((ln, co), s) -> 
	case s of
		 c : s' -> case c of
			'\n'-> Right (c, ((ln + 1, 0), s'))
			_ -> Right (c, ((ln, co + 1), s'))
		 [] -> Left "empty"

{-
k ('\n' : s)
k ('+' : s)
k ('|' : s)
k ('-' : s)
k ('>' : s)
k ('<' : s)
--k ('0' : '|' : s)
k ('0' : s) -- : '|'
k ('[' : s)
k ('(' : s)
k ('%' : s)
-}

test7 :: ((Int, Int), String) -> Either String (((Int, Int), String), Tok)
test7 = undefined

--rule: control statements ar followed by EOL
data Tok
--parts without mandatory horizontal component:
	= Node
	| VLine
--sole thing that occupy whole line
	| Label' Text -- "LABEL:"
--horizontal things
	| HLine -- Int --repetitions
	| REdge -- as block input "--->"
	| FEdge -- as block input "---<"
	| Negated -- on block i/o "---0|" or "|0---"
	| Contact Text -- "---[OP]---"
	| Coil Text -- "---(OP)---"
--as above, but could be mistaken for other things
	| Connector Text -- "--->NAME>"
	| Continuation Text -- ">NAME>---"
	| Return -- "---<RETURN>"
--Jump additionaly is followed by end of line
	| Jump' Text -- "--->>LABEL"
--others
-- 	| Store Text -- FBD only "---VARIABLE"
	| Name Text --inside of block
	deriving (Show, Eq)

test6 :: Parsec ParseErr Text [ (SourcePos, [((SourcePos, SourcePos), Tok)]) ]
test6 = many ln <* eof
	where
	ln :: Parsec ParseErr Text (SourcePos, [((SourcePos, SourcePos), Tok)])
	ln
		= leadingWhitespace
		*> ((,) <$> getSourcePos <*> (concat <$> some ln''))
		<* leadingWhitespace

	--ehm "not cheap"
-- 	tok_ :: Parsec ParseErr Text p -> Parsec ParseErr Text ((SourcePos, SourcePos), p)
	tok_ p = (\a b c -> ((a, c), b)) <$> getSourcePos <*> p <*> getSourcePos
	tok' p px
		= (:) <$> tok_ ( p) <*> px
	tok t p px
		= (:)
		<$> ((,t) <$> ((,) <$> getSourcePos <*> ( p *> getSourcePos)))
		<*> px

	ln'' :: Parsec ParseErr Text [((SourcePos, SourcePos), Tok)]
	ln''
		= some (tok_ $ Label' <$> try label' <* whitespace <* eol)
		<|> ln'

	ln' ::  Parsec ParseErr Text [((SourcePos, SourcePos), Tok)]
	ln'
		=
			(some (tok_ (VLine <$ (char '|')) <* whitespace))
		<|> (some $ tok_ (Node <$ (char '+' <* whitespace)))
		<|> (some $ tok_ $ Continuation <$> (try (between (char '>') (char '>') name)))

		<|> hline

		<|> (some $ tok_ (Name <$> name) <* whitespace)

	label' :: Parsec ParseErr Text Text
	label' = label "label" $ labelName <* char ':'
	labelName :: Parsec ParseErr Text Text
	labelName = T.pack <$> some alphaNumChar
	name :: Parsec ParseErr Text Text
-- 	name = label "identifier" $ T.pack <$> some (letterChar <|> char '%')
	name = label "identifier" $ T.pack <$> some (alphaNumChar <|> char '%')


	innards :: Parsec ParseErr Text Text
	innards = T.pack <$> some (satisfy (\c -> notElem c [')', ']']))

	--parse horizontal line (at least one dash)
	hline ::  Parsec ParseErr Text [((SourcePos, SourcePos), Tok)]
	hline
		=   tok HLine
				(some (char '-'))
			(concat <$> many hline')

	--parse things that are part of horizontal link (that is, after at least one dash)
	hline' ::  Parsec ParseErr Text [((SourcePos, SourcePos), Tok)]
	hline'
		=   (tok' (try (chunk ">>") *> (Label' <$> labelName)) (ln'') <* whitespace <* eol)
		<|> (tok' (Return <$ (try (between (chunk "<") (chunk ">") labelName)))
				(ln'')
				<* whitespace <* eol)
-- 		=   (:[]) <$> (tok_ (try (Label' <$> labelName)) <* whitespace <* eol)
		<|> tok' (Contact <$> (between (chunk "[") (chunk "]") innards)) hline
		<|> tok' (Coil <$> (between (chunk "(") (chunk ")") innards)) hline --FIXME do not require continuation of hline here
		<|> tok' (Connector <$> (between (char '>') (char '>') name)) (whitespace *> ln')

-- 		<|> tok REdge (char '>') hline'
-- 		<|> FEdge <$ char '<'
-- 		<|> Negated <$ char '0'
-- 		<|> [] <$ (eof <|> () <$ eol)
-- 		<|> ln'

	--eats comments and whitespace but not line endings
	whitespace = label "whitespace" $ white *> many (actualComment *> white) *> white
	--eats whitespace, comments and line endings
	leadingWhitespace :: Parsec ParseErr Text ()
	leadingWhitespace = label "comment'" $ () <$ (space *> many (actualComment *> space) *> space)

	actualComment = chunk "(*" *> manyTill anySingle (try (chunk "*)"))

	spaceButNotEOL = satisfy (\c -> isSpace c && c /= '\n')
	white = skipMany spaceButNotEOL
-- 	somewhite = () <$ some spaceButNotEOL
	

--now with columns stored i can eat tokens almost randomly
--TODO should also work for FBD
preproc4 :: Text -> Either Text [ (SourcePos, [((SourcePos, SourcePos), Tok)]) ]
preproc4 src =
	case parse test6 "(file)" src of
		 Left err ->
-- 				trace (errorBundlePretty err)
				Left $ T.pack $ errorBundlePretty err
		 Right n -> Right n

preproc4'' :: Text -> Either Text [(Int, [((Int, Int), Tok)])]
preproc4'' = fmap stripPos . preproc4

preproc4' :: String -> Either Text [(Int, [((Int, Int), Tok)])]
preproc4' = fmap stripPos . preproc4 . T.pack
-- test01 = preproc4 ""

stripPos
	:: [ (SourcePos, [((SourcePos, SourcePos), Tok)]) ]
	-> [ (Int, [((Int, Int), Tok)]) ]
stripPos = fmap (bimap (unPos.sourceLine)
	(fmap (bimap (bimap (unPos.sourceColumn) ((+(-1)).unPos.sourceColumn)) id)))

--------------------------------------------------------------------------------
