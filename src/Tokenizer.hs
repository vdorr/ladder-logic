{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
-- #define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Tokenizer where

-- import Data.Char

--FIXME FIXME get rid of megaparsec
import Text.Megaparsec --as P hiding (runParser', Pos)
import Text.Megaparsec.Char --as PC
-- import Text.Megaparsec.Char.Lexer (symbol)
import Data.Bifunctor
-- import Control.Monad hiding (fail)

import Control.Applicative.Combinators (between)

import qualified Data.Text as T
import Data.Text (Text)

-- import Text.Megaparsec.Debug
-- import Debug.Trace

import Preprocess (ParseErr(..), withPos)

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
contact'     : '[' anychar+ ']'
coil'        : '(' anychar+ ')'
connector    : '>' letter+ '>'
continuation : connector
return       : '<RETURN>'
jump         : '>>' target
name         : letter+
location     : '%' ('I' | 'Q' | 'M') number+

--linecomment  : '//' anychar* '\n'
blockcomment : '(*' anychar* '*)'
pragma       : '{' anychar* '}'

-}

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

-- chop by labels, does not look for labels floting among logic, that is left to parser
basicBlocks
    :: [[Tok a]]
    -> [(Maybe a, [[Tok a]])]
basicBlocks [] = []
basicBlocks t = (lbl, this) : basicBlocks rest
    where
    (this, rest) = break isLabel t'
    (lbl, t')
        = case t of
            ([Label' x] : xs) -> (Just x, xs)
            xs                -> (Nothing, xs)
    isLabel [Label' _] = True
    isLabel _          = False

isWsTok :: Tok a -> Bool
isWsTok Pragma{}  = True
isWsTok Comment{} = True
isWsTok _         = False

--rule: control statements (jump) are followed by EOL
data Tok a
--parts without mandatory horizontal component:
    = Node           -- +
    | VLine          -- |
--sole thing that occupy whole line
    | Label' a       -- "LABEL:"
--horizontal things
    | HLine          -- Int --repetitions
    | REdge          -- as block input "--->"
    | FEdge          -- as block input "---<"
    | Negated        -- on block i/o "---0|" or "|0---"
    | Contact !a     -- "---[OP]---"
    | Coil a         -- "---(OP)---"
--as above, but could be mistaken for other things
--     | Connector a        -- "--->NAME>"
    | Continuation a -- ">NAME>---" -- same as Connector
    | Return         -- "---<RETURN>"
--Jump additionaly is followed by end of line
    | Jump' a        -- "--->>LABEL"
--others
--     | Store a            -- FBD only "---VARIABLE"
    | Name a         --inside of block
--whitespace
    | Comment a
    | Pragma a
    deriving (Show, Eq, Functor)

token7 :: Parsec ParseErr Text (Tok Text)
token7
    =   Pragma       <$> between'' "{" "}"
    <|> Comment      <$> between'' "(*" "*)"
--     <|> Pragma       <$> T.pack <$> between' "{" "}" (many anySingle)
--     <|> Comment      <$> Comment <$> (chunk "(*" *> manyTill anySingle (try (chunk "*)")))

    <|> Label'       <$> try (labelName <* char ':')
    <|> Negated      <$  char '0'
    <|> VLine        <$  char '|'
    <|> Node         <$  char '+'
    <|> Continuation <$> try (between' ">" ">" name)
    <|> HLine        <$  some (char '-')
    <|> Jump'        <$> (try (chunk ">>") *> labelName)
--         <|> Return            <$  try (between' "<" ">" labelName)
    <|> Return       <$  try (chunk "<RETURN>")
--     <|> Contact      <$> between' "[" "]" innards
--     <|> Coil         <$> between' "(" ")" innards
    <|> Contact      <$> between'' "[" "]"
    <|> Coil         <$> between'' "(" ")"
--         <|> Connector        <$> try (between ">" ">" name)
    <|> REdge        <$  char '>'
    <|> FEdge        <$  char '<'
    <|> Name         <$> name

    where
    labelName = T.pack <$> some alphaNumChar
    name = label "identifier" $ T.pack <$> some (alphaNumChar <|> char '%')
--     innards = T.pack <$> some (satisfy (\c -> notElem c [')', ']']))
    between' a b = between (chunk a) (chunk b)
    between'' :: Text -> Text -> Parsec ParseErr Text Text
    between'' a b = T.pack <$> (chunk a *> manyTill anySingle (try (chunk b)))

test7' :: Parsec ParseErr Text [((SourcePos, SourcePos), Tok Text)]
test7' = space *> many (withPos token7 <* space) <* eof

test7 :: Parsec ParseErr Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
test7 = (breakLines . filter (not.isWsTok.snd)) <$> test7'

breakLines
    :: [((SourcePos, SourcePos), Tok Text)]
    -> [(SourcePos, [((SourcePos, SourcePos), Tok Text)])]
breakLines (x@((p, _), _) : xs) = (p, x : a) : breakLines b
    where
    (a, b) = span ((sourceLine p==).sourceLine.fst.fst) xs
breakLines [] = []

preproc5'
    :: Text
    -> Either Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
preproc5'
    = bimap (T.pack . errorBundlePretty) id
    . parse test7 "(file)"
 
-- whitespace7 :: Parsec ParseErr Text ()
-- whitespace7 = whitespace
--     where
--     whitespace = label "whitespace" $ space *> many (actualComment *> space) *> space
--     actualComment = chunk "(*" *> manyTill anySingle (try (chunk "*)"))

-- preproc5 :: Text -> Either Text [((SourcePos, SourcePos), Tok Text)]
-- preproc5
--     = bimap (T.pack . errorBundlePretty) id
--     . parse test7' "(file)"

--------------------------------------------------------------------------------
