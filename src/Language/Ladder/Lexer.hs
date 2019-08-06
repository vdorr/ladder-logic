{-# LANGUAGE OverloadedStrings #-}

module Language.Ladder.Lexer where

--FIXME FIXME get rid of megaparsec
import Text.Megaparsec hiding (Label) --as P hiding (runParser', Pos)
import Text.Megaparsec.Char --as PC
import Data.Bifunctor
import Control.Applicative.Combinators (between)

import qualified Data.Text as T
import Data.Text (Text)

--------------------------------------------------------------------------------

newtype ParseErr = PE String
    deriving (Show, Eq, Ord)

instance ShowErrorComponent ParseErr where
    showErrorComponent (PE e)  = show e

--------------------------------------------------------------------------------

withPos :: (Stream t, Ord e) => Parsec e t a -> Parsec e t ((SourcePos, SourcePos), a)
withPos p = (\a b c -> ((a, c), b)) <$> getSourcePos <*> p <*> getSourcePos

--------------------------------------------------------------------------------

-- lex2 :: (s -> (Maybe Char, s)) -> s -> Either String [Tok x]
-- lex2 uncons s = error "TODO"

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
{-

├ ─

├─

      ├─
│ │   └─

-}

-- |Diagram token
--rule: control statements (jump) are followed by EOL
data Tok a
--parts without mandatory horizontal component:
    = Cross            -- ^ "+"
    | VLine            -- ^ "|"
--sole thing that occupy whole line
    | Label        !a   -- ^ network label "LABEL:"
--horizontal things
    | HLine        !Int !Int --number of '-' consumed, number of following '|' seen
    | REdge            -- ^ as block input "--->"
    | FEdge            -- ^ as block input "---<"

--     | Negated        -- on block i/o "---0|" or "|0---"
--TODO count leading zeroes, or, store total number of chars - good for `lexemeLength`
    | Number       !Int --should probably be of type 'a'
    | Contact      !a   -- ^ "---[OP]---"
    | Coil         !a   -- ^ "---(OP)---"

--as above, but could be mistaken for other things
--     | Connector a        -- "--->NAME>"
    | Continuation !a   -- ^ ">NAME>---" -- same as Connector
    | Return           -- ^ "---\<RETURN>"
--Jump additionaly is followed by end of line
    | Jump'        !a   -- ^ "--->>LABEL"

--others
--     | Store a            -- FBD only "---VARIABLE"
    | Name         !a   -- ^inside of block
--whitespace
    | Comment      ![a]   -- ^ (* ... *)
    | Pragma       ![a]   -- ^ { ... }
    | NewLine
    | Whitespace   !Int
    deriving (Show, Eq, Functor)

--------------------------------------------------------------------------------

renderLexeme :: Tok String -> String
renderLexeme t = case t of
    Cross            -> "+"
    VLine            -> "|"
    Label        a   -> a <> ":"
    HLine        n _ -> replicate (n+1) '-'
    REdge            -> ">"
    FEdge            -> "<"
    Number       n   -> show n
    Contact      a   -> "[" <> a <> "]"
    Coil         a   -> "(" <> a <> ")"
    Continuation a   -> ">" <> a <> ">"
    Return           -> "<RETURN>"
    Jump'        a   -> ">>" <> a
    Name         a   -> a
    Comment      a   -> "(*" <> mconcat a <> "*)"
    Pragma       a   -> "{" <> mconcat a <> "}"
    NewLine          -> "\n" --FIXME windows
    Whitespace   n   -> replicate n ' '

-- lexemeLength :: Tok String -> (Int, Int)
-- lexemeLength t = case t of
--     Cross            -> (0, 1)
--     VLine            -> (0, 1)
--     Label        a   -> (0, 1 + length a)
--     HLine        n _ -> (0, n + 1)
--     REdge            -> (0, 1)
--     FEdge            -> (0, 1)
--     Number       n   -> (0, length $ show n)
--     Contact      a   -> (0, 2 + length a)
--     Coil         a   -> (0, 2 + length a)
--     Continuation a   -> (0, 2 + length a)
--     Return           -> (0, 8)
--     Jump'        a   -> (0, 2 + length a)
--     Name         a   -> (0, length a)
--     Comment      a   -> (length a - 1, 4 + (length $ unlines a))
--     Pragma       a   -> (length a - 1, 2 + (length $ unlines a))
--     NewLine          -> (1, 0)
--     Whitespace   n   -> (0, n)

--------------------------------------------------------------------------------

lexeme :: Parsec ParseErr Text (Tok Text)
lexeme
    =   Pragma       <$> T.lines <$> between''' "{" "}"
    <|> Comment      <$> T.lines <$> between''' "(*" "*)"
    <|> Whitespace   <$> length <$> some (char ' ')
    <|> NewLine      <$  eol
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
    name = label "identifier" $ T.pack <$> some (alphaNumChar <|> char '_' <|> char '%')
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
-- lexerP = space *> many (withPos lexeme <* space) <* eof
lexerP = many (withPos lexeme) <* eof

lexerLinesP :: Parsec ParseErr Text [(SourcePos, [((SourcePos, SourcePos), Tok Text)])]
lexerLinesP = breakLines <$> lexerP

runLexer
    :: Text
    -> Either Text [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
runLexer
    = bimap (T.pack . errorBundlePretty) id
    . parse lexerLinesP "(file)"

runLexer'
    :: Text
    -> Either String [ (SourcePos, [((SourcePos, SourcePos), Tok Text)]) ]
runLexer'
    = first errorBundlePretty
    . parse lexerLinesP "(file)"

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
isWsTok Pragma {}    = True
isWsTok Comment{}    = True
isWsTok Whitespace{} = True
isWsTok NewLine      = True
isWsTok _            = False

-- |Chop by network labels
--TODO keep source pos for start of block
-- does not look for labels floating among logic, that is left to parser
-- produced list of (labeled) networks
labeledRungs
    :: [(p, [((p, p), Tok a)])]
    -> [(Maybe a, [(p, [((p, p), Tok a)])])]
labeledRungs [] = []
labeledRungs t = (lbl, this) : labeledRungs rest
    where
    (this, rest) = break isLabel t'
    (lbl, t')
        = case t of
            ((_, [(_, Label x)]) : xs) -> (Just x, xs)
            xs                         -> (Nothing, xs)

    isLabel (_, [(_, Label _)]) = True
    isLabel _         = False

--------------------------------------------------------------------------------

-- |Discard 'SourcePos', keep only integer line numbers and column ranges
stripPos
    :: [ (SourcePos, [((SourcePos, SourcePos), a)]) ]
    -> [ (Int, [((Int, Int), a)]) ]
stripPos = fmap (bimap (unPos.sourceLine)
    (fmap (first (bimap (unPos.sourceColumn) ((+(-1)).unPos.sourceColumn)))))

--------------------------------------------------------------------------------

-- |Look for first pragma in list of lexemes
-- getPragma :: [Tok a] -> Maybe [a]
-- getPragma xs = case getLeadingPragmas xs of
--     x : _ -> Just x
--     _     -> Nothing

-- |Look for first pragma in list of lexemes
getLeadingPragmas :: [Tok a] -> [[a]]
getLeadingPragmas = go
    where
    go (Pragma  p    : xs) = p : go xs
    go (Comment _    : xs) =     go xs
    go (Whitespace _ : xs) =     go xs
    go (NewLine      : xs) =     go xs
    go _                   = []

-- |Discard position informations from list of lexemes
dropPos
    :: [(p, [((p, p), Tok a)])]
    -> [Tok a]
dropPos = foldMap (fmap snd . snd)

--------------------------------------------------------------------------------
