{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Language.Ladder.Lexer where

--FIXME FIXME get rid of megaparsec
import Text.Megaparsec hiding (Label) --as P hiding (runParser', Pos)
import Text.Megaparsec.Char --as PC
import Data.Bifunctor
import Control.Applicative.Combinators (between)

import qualified Data.Text as T
import Data.Text (Text)

--------------------------------------------------------------------------------

-- import Preprocess (ParseErr(..), withPos)
type ParseErr = String

instance ShowErrorComponent ParseErr where
    showErrorComponent  = show

--------------------------------------------------------------------------------

withPos :: Parsec ParseErr Text a -> Parsec ParseErr Text ((SourcePos, SourcePos), a)
withPos p = (\a b c -> ((a, c), b)) <$> getSourcePos <*> p <*> getSourcePos

--------------------------------------------------------------------------------

{-
letter       : 'A'..'Z' | '_'
number       : '0'..'9'

token        : cross | vline | hline | label ...

target       : number+ | letter+
cross         : '+'
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


-- |Diagram token
--rule: control statements (jump) are followed by EOL
data Tok a
--parts without mandatory horizontal component:
    = Cross            -- ^ "+"
    | VLine            -- ^ "|"
--sole thing that occupy whole line
    | Label        a   -- ^ network label "LABEL:"
--horizontal things
    | HLine        Int Int --number of '-' consumed, number of following '|' seen
    | REdge            -- ^ as block input "--->"
    | FEdge            -- ^ as block input "---<"

--     | Negated        -- on block i/o "---0|" or "|0---"
    | Number       Int --should probably be of type 'a'
    | Contact      a   -- ^ "---[OP]---"
    | Coil         a   -- ^ "---(OP)---"

--as above, but could be mistaken for other things
--     | Connector a        -- "--->NAME>"
    | Continuation a   -- ^ ">NAME>---" -- same as Connector
    | Return           -- ^ "---\<RETURN>"
--Jump additionaly is followed by end of line
    | Jump'        a   -- ^ "--->>LABEL"

--others
--     | Store a            -- FBD only "---VARIABLE"
    | Name         a   -- ^inside of block
--whitespace
    | Comment      a   -- ^ (* ... *)
    | Pragma       a   -- ^ { ... }
    deriving (Show, Eq, Functor)

--------------------------------------------------------------------------------

renderLexeme :: Tok String -> String
renderLexeme t = case t of
    Cross            -> "+"
    VLine            -> "|"
    Label        a   -> a ++ ":"
    HLine        n _ -> replicate (n+1) '-'
    REdge            -> ">"
    FEdge            -> "<"
    Number       n   -> show n
    Contact      a   -> "[" ++ a ++ "]"
    Coil         a   -> "(" ++ a ++ ")"
    Continuation a   -> ">" ++ a ++ ">"
    Return           -> "<RETURN>"
    Jump'        a   -> ">>" ++ a
    Name         a   -> a
    Comment      a   -> "(*" ++ a ++ "*)"
    Pragma       a   -> "{" ++ a ++ "}"

lexeme :: Parsec ParseErr Text (Tok Text)
lexeme
    =   Pragma       <$> between''' "{" "}"
    <|> Comment      <$> between''' "(*" "*)"
    <|> Label        <$> try (labelName <* char ':')
--     <|> Negated      <$  char '0'
    <|> Number       <$> (read <$> some digitChar)
    <|> VLine        <$  char '|'
    <|> Cross        <$  char '+'
    <|> Continuation <$> try (between' ">" ">" name)

    <|> HLine        <$> (((+(-1)).length) <$> some (char '-'))
                     <*> (lookAhead (length <$> many (char '|')))

    <|> Jump'        <$> (try (chunk ">>") *> labelName)
    <|> Return       <$  try (chunk "<RETURN>")
    <|> Contact      <$> between'' "[" "]"
    <|> Coil         <$> between'' "(" ")"
--         <|> Connector        <$> try (between ">" ">" name)
    <|> REdge        <$  char '>'
    <|> FEdge        <$  char '<'
    <|> Name         <$> name

    where
    labelName = T.pack <$> some alphaNumChar
    name = label "identifier" $ T.pack <$> some (alphaNumChar <|> char '%')
    between' a b = between (chunk a) (chunk b)
    between'' :: Text -> Text -> Parsec ParseErr Text Text
    between'' a b = T.pack <$> (chunk a *> manyTill anySingle (try (chunk b)))

    --escape with backlsash, allow curly braces in pragmas
    between''' :: Text -> Text -> Parsec ParseErr Text Text
    between''' a b = mconcat <$> (chunk a *> manyTill interm (try (chunk b)))
        where
        interm = do
            c <- anySingle
            if '\\' == c
            then chunk b <|> return (T.singleton c)
            else return (T.singleton c)

lexerP :: Parsec ParseErr Text [((SourcePos, SourcePos), Tok Text)]
lexerP = space *> many (withPos lexeme <* space) <* eof

lexerLinesP :: Parsec ParseErr Text [(SourcePos, [((SourcePos, SourcePos), Tok Text)])]
lexerLinesP = breakLines <$> lexerP

runLexer
    :: Text
    -> Either Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
runLexer
    = bimap (T.pack . errorBundlePretty) id
    . parse lexerLinesP "(file)"

--------------------------------------------------------------------------------

test7 :: Parsec ParseErr Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
test7 = (breakLines . filter (not.isWsTok.snd)) <$> lexerP

preproc5'
    :: Text
    -> Either Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
preproc5'
    = bimap (T.pack . errorBundlePretty) id
    . parse test7 "(file)"

--------------------------------------------------------------------------------

-- |Discard comments and pragmas
dropWhitespace
    :: [(p, [((p, p), Tok a)])]
    -> [(p, [((p, p), Tok a)])]
dropWhitespace = filter (not.null.snd) . fmap (fmap (filter (not.isWsTok.snd)))

-- |Break list of tokens into list of lists of tokens with same line number
breakLines
    :: [((SourcePos, SourcePos), tok)]
    -> [(SourcePos, [((SourcePos, SourcePos), tok)])]
breakLines (x@((p, _), _) : xs) = (p, x : a) : breakLines b
    where
    (a, b) = span ((sourceLine p==).sourceLine.fst.fst) xs
breakLines [] = []

-- |Returns True if lexeme is comment or pragma
isWsTok :: Tok a -> Bool
isWsTok Pragma{}  = True
isWsTok Comment{} = True
isWsTok _         = False

-- |Chop by network labels
--TODO keep source pos for start of block
-- does not look for labels floating among logic, that is left to parser
-- produced list of (labeled) networks
basicBlocks'
    :: [(p, [((p, p), Tok a)])]
    -> [(Maybe a, [(p, [((p, p), Tok a)])])]
basicBlocks' [] = []
basicBlocks' t = (lbl, this) : basicBlocks' rest
    where
    (this, rest) = break isLabel t'
    (lbl, t')
        = case t of
            ((_, [(_, Label x)]) : xs) -> (Just x, xs)
            xs                -> (Nothing, xs)
    isLabel (_, [(_, Label _)]) = True
    isLabel _         = False

basicBlocks
    :: [[Tok a]]
    -> [(Maybe a, [[Tok a]])]
basicBlocks
    = fmap (fmap (fmap (snd . fmap(fmap snd))))
    . basicBlocks'
    . (fmap (((),).fmap (((),()),)))

--------------------------------------------------------------------------------

-- |Discard 'SourcePos', keep only integer line numbers and column ranges
stripPos
    :: [ (SourcePos, [((SourcePos, SourcePos), a)]) ]
    -> [ (Int, [((Int, Int), a)]) ]
stripPos = fmap (bimap (unPos.sourceLine)
    (fmap (bimap (bimap (unPos.sourceColumn) ((+(-1)).unPos.sourceColumn)) id)))

--------------------------------------------------------------------------------
