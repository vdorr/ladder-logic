{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}
-- #define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Preprocess where

import Data.Char

import Text.Megaparsec --as P hiding (runParser', Pos)
import Text.Megaparsec.Char --as PC
-- import Text.Megaparsec.Char.Lexer (symbol)
--import Data.Bifunctor
-- import Control.Monad hiding (fail)

-- import Control.Applicative.Combinators (between)

import qualified Data.Text as T
import Data.Text (Text)

-- import Text.Megaparsec.Debug
-- import Debug.Trace
import Language.Ladder.Lexer (ParseErr(..)) --stripPos, , withPos

--------------------------------------------------------------------------------

--FIXME FIXME add some positional info (at least line numbers)

--TODO nice leading column
--maybe parse even rungs

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

-- type LexSt = ((Int, Int), String)
-- 
-- newtype Lexer a = Lexer { lexer :: LexSt -> Either String (a, LexSt) }
-- 
-- instance Functor Lexer where
-- 	fmap = ap . return
-- 
-- instance Applicative Lexer where
-- 	pure = return
-- 	(<*>) = ap
-- 
-- instance Monad Lexer where
-- 	return a = Lexer $ \s -> return (a, s)
-- 	a >>= b = Lexer $ \s -> do
-- 		(y, s') <- lexer a s
-- 		lexer (b y) s'
-- 
-- getOne :: Lexer Char
-- getOne = Lexer $ \((ln, co), s) -> 
-- 	case s of
-- 		 c : s' -> case c of
-- 			'\n'-> Right (c, ((ln + 1, 0), s'))
-- 			_ -> Right (c, ((ln, co + 1), s'))
-- 		 [] -> Left "empty"

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

#if 0
-- chop by labels, does not look for labels floting among logic, that is left to parser
basicBlocks :: [[Tok a]] -> [(Maybe a, [[Tok a]])]
basicBlocks [] = []
basicBlocks t = (lbl, this) : basicBlocks rest
	where
	(this, rest) = break isLabel t'
	(lbl, t')
		= case t of
			([Label' x] : xs)	-> (Just x, xs)
			xs					-> (Nothing, xs)
	isLabel [Label' _] = True
	isLabel _ = False

-- test8 :: ((Int, Int), String) -> Either String (((Int, Int), String), Tok)
-- test8 = undefined

--rule: control statements ar followed by EOL
data Tok a
--parts without mandatory horizontal component:
	= Node				-- +
	| VLine				-- |
--sole thing that occupy whole line
	| Label' a			-- "LABEL:"
--horizontal things
	| HLine				-- Int --repetitions
	| REdge				-- as block input "--->"
	| FEdge				-- as block input "---<"
	| Negated			-- on block i/o "---0|" or "|0---"
	| Contact a			-- "---[OP]---"
	| Coil a			-- "---(OP)---"
--as above, but could be mistaken for other things
-- 	| Connector a		-- "--->NAME>"
	| Continuation a	-- ">NAME>---" -- same as Connector
	| Return			-- "---<RETURN>"
--Jump additionaly is followed by end of line
	| Jump' a			-- "--->>LABEL"
--others
-- 	| Store a			-- FBD only "---VARIABLE"
	| Name a			--inside of block
	deriving (Show, Eq, Functor)

token7 :: Parsec ParseErr Text (Tok Text)
token7 = tok
	where
	tok
		=   Label'			<$> try (labelName <* char ':')
		<|> VLine			<$  char '|'
		<|> Node			<$  char '+'
		<|> Continuation	<$> try (between' ">" ">" name)
		<|> HLine			<$  some (char '-')
		<|> Jump'			<$> (try (chunk ">>") *> labelName)
-- 		<|> Return			<$  try (between' "<" ">" labelName)
		<|> Return			<$  try (chunk "<RETURN>")
		<|> Contact			<$> between' "[" "]" innards
		<|> Coil			<$> between' "(" ")" innards
-- 		<|> Connector		<$> try (between ">" ">" name)
-- 		<|> REdge			<$  (char '>')
-- 		<|> FEdge			<$  char '<'
-- 		<|> Negated			<$  char '0'
		<|> Name			<$> name

-- 	labelName :: Parsec ParseErr Text Text
	labelName = T.pack <$> some alphaNumChar
-- 	name :: Parsec ParseErr Text Text
-- 	name = label "identifier" $ T.pack <$> some (letterChar <|> char '%')
	name = label "identifier" $ T.pack <$> some (alphaNumChar <|> char '%')

-- 	innards :: Parsec ParseErr Text Text
	innards = T.pack <$> some (satisfy (\c -> notElem c [')', ']']))

	between' a b = between (chunk a) (chunk b)

whitespace7 :: Parsec ParseErr Text ()
whitespace7 = whitespace
	where
	whitespace = label "whitespace" $ space *> many (actualComment *> space) *> space
	actualComment = chunk "(*" *> manyTill anySingle (try (chunk "*)"))

test7' :: Parsec ParseErr Text [((SourcePos, SourcePos), Tok Text)]
test7' = whitespace7 *> many (withPos token7 <* whitespace7) <* eof

test7 :: Parsec ParseErr Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
test7 = f <$> test7'
	where
	f (x@((p, _), _) : xs) = (p, x : a) : f b
		where
		(a, b) = break ((sourceColumn p==).sourceColumn.fst.fst) xs
	f [] = []

--preproc5 :: Text -> Either Text [ (SourcePos, [((SourcePos, SourcePos), Tok)]) ]
preproc5 :: Text -> Either Text [((SourcePos, SourcePos), Tok Text)]
preproc5 src
-- 	= case parse test7' "(file)" src of
-- 		 Left err ->
-- -- 				trace (errorBundlePretty err)
-- 				Left $ T.pack $ errorBundlePretty err
-- 		 Right n -> Right n
	= bimap (T.pack . errorBundlePretty) id
	$ parse test7' "(file)" src
#endif

--------------------------------------------------------------------------------

#if 0
test6 :: Parsec ParseErr Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
test6 = many ln <* eof
	where
	ln :: Parsec ParseErr Text (SourcePos, [((SourcePos, SourcePos), Tok Text)])
	ln
		= leadingWhitespace
		*> ((,) <$> getSourcePos <*> (concat <$> some ln''))
		<* leadingWhitespace

	--ehm "not cheap"
-- 	tok_ :: Parsec ParseErr Text p -> Parsec ParseErr Text ((SourcePos, SourcePos), p)
	tok_ = withPos -- p = (\a b c -> ((a, c), b)) <$> getSourcePos <*> p <*> getSourcePos
	tok' p px
		= (:) <$> tok_ ( p) <*> px
	tok t p px
		= (:)
		<$> ((,t) <$> ((,) <$> getSourcePos <*> ( p *> getSourcePos)))
		<*> px

	ln'' :: Parsec ParseErr Text [((SourcePos, SourcePos), Tok Text)]
	ln''
		= some (tok_ $ Label' <$> try label' <* whitespace <* eol)
		<|> ln'

	ln' ::  Parsec ParseErr Text [((SourcePos, SourcePos), Tok Text)]
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
	hline ::  Parsec ParseErr Text [((SourcePos, SourcePos), Tok Text)]
	hline
		=   tok HLine
				(some (char '-'))
			(concat <$> many hline')

	--parse things that are part of horizontal link (that is, after at least one dash)
	hline' ::  Parsec ParseErr Text [((SourcePos, SourcePos), Tok Text)]
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
preproc4 :: Text -> Either Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
preproc4 src =
	case parse test7 "(file)" src of
		 Left err ->
-- 				trace (errorBundlePretty err)
				Left $ T.pack $ errorBundlePretty err
		 Right n -> Right n

preproc4'' :: Text -> Either Text [(Int, [((Int, Int), Tok Text)])]
preproc4'' = fmap stripPos . preproc4

-- preproc4' :: String -> Either Text [(Int, [((Int, Int), Tok Text)])]
-- preproc4' = fmap stripPos . preproc4 . T.pack
#endif

--------------------------------------------------------------------------------
