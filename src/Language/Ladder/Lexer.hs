
module Language.Ladder.Lexer
    ( Tok(..)
    , dropWhitespace2
    , stripPos3
    , runLexer'
    , labeledRungs
    , runLexer
    , getLeadingPragmas
    , dropPos

    , renderLexeme
    , dropPos2
    , lx
    )
    where

--FIXME FIXME get rid of megaparsec
import Text.Megaparsec hiding (Label) --as P hiding (runParser', Pos)
import Text.Megaparsec.Char --as PC
import Data.Bifunctor
import Control.Applicative.Combinators (between)
import Data.List
import qualified Data.Text as T
import Data.Text (Text)

import Data.Char

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

-- |Diagram token
data Tok a
    = Cross            -- ^ "+"
    | VLine            -- ^ "|"
    | Label        !a   -- ^ network label "LABEL:"
    | HLine        !Int !Int -- ^ number of "-" consumed, number of following '|' seen
    | REdge            -- ^ as block input "--->"
    | FEdge            -- ^ as block input "---<"

--TODO count leading zeroes, or, store total number of chars - good for `lexemeLength`
    | Number       !Int -- ^integer number

    | Contact      !a   -- ^ "---[OP]---"
    | Coil         !a   -- ^ "---(OP)---"

--as above, but could be mistaken for other things
--     | Connector a        -- "--->NAME>"
    | Continuation !a   -- ^ ">NAME>---", same as Connector
    | Return           -- ^ "---\<RETURN>"
--Jump additionaly is followed by end of line
    | Jump'        !a   -- ^ "--->>LABEL"
    | Name         !a   -- ^device operand or block name
    | Comment      ![a]   -- ^ (* ... *)
    | Pragma       ![a]   -- ^ { ... }
    | NewLine
    | Whitespace   !Int
    | Colon
--     | Store a            -- FBD only "---VARIABLE"
    deriving (Show, Eq, Functor)

--------------------------------------------------------------------------------

-- lx :: String -> Either String ((Int, [((Int, Int), Tok String)]), String)
lx :: String -> Either String [((Int, Int), Tok String)]
lx s = fmap (\((_, q), _) -> reverse q) $ f s (0, [])
    where
--     len = length s
    f    ('{':     xs) t = lol xs (Pragma . lines)  (takeUntilC '}') t
    f    ('(':'*': xs) t = lol xs (Comment . lines) (takeUntil "*)") t
    f xs@(' ':    _xs) t = lol xs (Whitespace . (length)) (chars ' ') t
    f    ('\n':    xs) t = lol xs (const NewLine) (const $ Right ((), xs)) t
    f    ('|':     xs) t = lol xs (const VLine) (const $ Right ((), xs)) t
    f    ('+':     xs) t = lol xs (const Cross) (const $ Right ((), xs)) t
    f    ('-':     xs) t = lol' xs (\a b -> HLine (length a - 1) (countvl b)) (chars '-') t
    f    ('[':     xs) t = lol xs (Contact) (takeUntil "]" ) t
    f    ('(':     xs) t = lol xs (Coil)  (takeUntil ")" ) t
    f    ('>':     xs) t = lol xs (const REdge) (const $ Right ((), xs)) t
    f    ('<':     xs) t = lol xs (const FEdge) (const $ Right ((), xs)) t
    f    (':':     xs) t = lol xs (const Colon) (const $ Right ((), xs)) t
    f    (c  :     xs) t
        | isDigit c      = lol xs (Number . read)  (digits) t
        | isAlpha c      = lol xs (Name)  (alphaNum) t
    f    []            t = Right (t, [])
    f (other:       _) _ = Left ("unexpected char '" ++ [other] ++ "'")

    lol :: String -> (w -> Tok String)
        -> (String -> Either String (w, String))
        -> (Int, [((Int, Int), Tok String)])
        -> Either String ((Int, [((Int, Int), Tok String)]), String)
    lol' :: String -> (w -> String -> Tok String)
        -> (String -> Either String (w, String))
        -> (Int, [((Int, Int), Tok String)])
        -> Either String ((Int, [((Int, Int), Tok String)]), String)
    lol xs g p ys = 
        lol' xs (\a _ -> g a) p ys
--         undefined
    lol' xs g p (k, ys) = do
        (a, xs') <- p xs
        let k' = k+length xs - length xs'
        f xs' (k', ((k, k'), g a xs') : ys) -- g a xs' : ys)
--         undefined

    countvl xs = length $ takeWhile (=='|') xs
    digits cs = Right $ span (isDigit) cs
    alphaNum cs = Right $ span (isAlphaNum) cs
    chars c cs = Right $ span (==c) cs -- or until, not sure
    takeUntil n h = case T.breakOn (T.pack n) (T.pack h) of
                     (p, xs) -> case T.stripPrefix (T.pack n) xs of
                                     Nothing -> Left undefined
                                     Just xxs -> Right (T.unpack p, T.unpack xxs)
    takeUntilC n h = case break (==n) (h) of
                     (p, n':xs) | n'==n, isSuffixOf "\\" p
                        -> first ((p++[n'])++) <$> takeUntilC n xs
                     (p, n':xs) | n'==n -> Right (p, xs)
                     (_p, _xs) -> Left "lol"

lxx :: [((Int, Int), Tok String)] -> [((Int, Int), Tok String)]
lxx = f
    where
    f ((a, Name lbl) : (b, Colon) : xs) = (ext a b, Label lbl) : f xs
    f ((a, Number n) : (b, Colon) : xs) = (ext a b, Label (show n)) : f xs --FIXME jump to number
    f ((a, REdge) : (_, REdge) : (b, Name _) : xs) = (ext a b, Jump' undefined) : f xs
    f ((a, FEdge) : (_, Name ret) : (b, REdge) : xs)
        | fmap toLower ret == "return" = (ext a b, Return) : f xs
    f (x:xs) = x : f xs
    f [] = []
    ext (a, _) (_, b)  = (a, b)

--------------------------------------------------------------------------------

lexeme :: Parsec ParseErr Text (Tok Text)
lexeme
    =   Pragma       <$> T.lines <$> between''' "{" "}"
    <|> Comment      <$> T.lines <$> between''' "(*" "*)"
    <|> Whitespace   <$> length <$> some (char ' ')
    <|> NewLine      <$  eol
    <|> Label        <$> try (labelName <* char ':')
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

lexerLinesP :: Parsec ParseErr Text [[((SourcePos, SourcePos), Tok Text)]]
lexerLinesP = (fmap snd . breakLines) <$> lexerP

--------------------------------------------------------------------------------

runLexer
    :: Text
    -> Either Text [ [((SourcePos, SourcePos), Tok Text)] ]
runLexer
    = bimap (T.pack . errorBundlePretty) id
    . parse lexerLinesP "(file)"

runLexer'
    :: Text
    -> Either String [ [((SourcePos, SourcePos), Tok Text)] ]
runLexer'
    = first errorBundlePretty
    . parse lexerLinesP "(file)"

--------------------------------------------------------------------------------

-- |Discard comments and pragmas
-- dropWhitespace
--     :: [(p, [((p, p), Tok a)])]
--     -> [(p, [((p, p), Tok a)])]
-- dropWhitespace = filter (not.null.snd) . fmap (fmap (filter (not.isWsTok.snd)))

-- |Discard comments and pragmas
dropWhitespace2
    :: [[(p, Tok a)]]
    -> [[(p, Tok a)]]
dropWhitespace2 = filter (not.null) . fmap (filter (not.isWsTok.snd))

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
isWsTok Pragma    {} = True
isWsTok Comment   {} = True
isWsTok Whitespace{} = True
isWsTok NewLine      = True
isWsTok _            = False

-- |Chop by network labels
--TODO keep source pos for start of block
-- does not look for labels floating among logic, that is left to parser
-- produced list of (labeled) networks
labeledRungs
    :: [[(p, Tok a)]]
    -> [(Maybe a, [[(p, Tok a)]])]
labeledRungs [] = []
labeledRungs t = (lbl, this) : labeledRungs rest
    where
    (this, rest) = break isLabel t'
    (lbl, t')
        = case t of
            ([(_, Label x)] : xs) -> (Just x, xs)
            xs                         -> (Nothing, xs)

    isLabel [(_, Label _)] = True
    isLabel _                   = False

--------------------------------------------------------------------------------

-- |Discard 'SourcePos', keep only integer line numbers and column ranges
-- stripPos
--     :: [ (SourcePos, [((SourcePos, SourcePos), a)]) ]
--     -> [ (Int, [((Int, Int), a)]) ]
-- stripPos = fmap (bimap (unPos.sourceLine)
--     (fmap (first (bimap (unPos.sourceColumn) ((+(-1)).unPos.sourceColumn)))))
-- 
-- stripPos2
--     :: [ (SourcePos, [((SourcePos, SourcePos), a)]) ]
--     -> [[((Int, (Int, Int)), a)]]
-- stripPos2 = fmap (fmap (first line) . snd)
--     where
--     line (s, e) = (unPos$sourceLine s, (unPos$sourceColumn s, (unPos$sourceColumn e)-1))

stripPos3
    :: [ [((SourcePos, SourcePos), a)] ]
    -> [[((Int, (Int, Int)), a)]]
stripPos3 = fmap (fmap (first line))
    where
    line (s, e) = (unPos$sourceLine s, (unPos$sourceColumn s, (unPos$sourceColumn e)-1))

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
    :: [[(p, Tok a)]]
    -> [Tok a]
dropPos = foldMap (fmap snd)

dropPos2
    :: [[(p, Tok a)]]
    -> [[Tok a]]
dropPos2 = fmap (fmap snd)

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
    Colon            -> ":"

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
