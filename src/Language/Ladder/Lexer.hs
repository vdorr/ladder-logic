
module Language.Ladder.Lexer
    ( Tok(..)
    , dropWhitespace2
--     , stripPos3
    , runLexer'
    , labeledRungs
    , runLexer
    , getLeadingPragmas
--     , dropPos

    , renderLexeme
--     , dropPos2
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

type SrcRange = (Int, (Int, Int))

-- lx :: String -> Either String [(SrcRange, Tok String)]
-- lx s = fmap (\((_, _, q), _) -> reverse q) $ f s (1, 1, [])
--     where
--     f    ('{':     xs) t = lol xs (Pragma . lines)  (takeUntilC '}') t
--     f    ('(':'*': xs) t = lol xs (Comment . lines) (takeUntil "*)") t
--     f xs@(' ':    _xs) t = lol xs (Whitespace . (length)) (chars ' ') t
--     f    ('\n':    xs) t = lol' (+1) (+1) xs (\_ _ -> NewLine) (const $ Right ((), xs)) t
--     f    ('|':     xs) t = lol xs (const VLine) (const $ Right ((), xs)) t
--     f    ('+':     xs) t = lol xs (const Cross) (const $ Right ((), xs)) t
--     f    ('-':     xs) t = lol' (+1) id xs (\a b -> HLine (length a + 1) (countvl b)) (chars '-') t
--     f    ('[':     xs) t = lol xs (Contact) (takeUntil "]" ) t
--     f    ('(':     xs) t = lol xs (Coil)  (takeUntil ")" ) t
--     f    ('>':     xs) t = lol xs (const REdge) (const $ Right ((), xs)) t
--     f    ('<':     xs) t = lol xs (const FEdge) (const $ Right ((), xs)) t
--     f    (':':     xs) t = lol xs (const Colon) (const $ Right ((), xs)) t
--     f xs@(c  :     _)  t
--         | isDigit c      = lol xs (Number . read)  (digits) t
--         | isAlpha c || c=='%'     = lol xs (Name)  (alphaNum) t
--     f    []            t = Right (t, [])
--     f (other:       _) _ = Left ("unexpected char '" ++ [other] ++ "'")
-- 
--     lol' :: (Int -> Int) -> (Int -> Int) -> String -> (w -> String -> Tok String)
--         -> (String -> Either String (w, String))
--         -> (Int, Int, [(SrcRange, Tok String)])
--         -> Either String
--             ((Int, Int, [(SrcRange, Tok String)]), String)
--     lol' mapCol mapLine xs g p (ln, k, ys) = do
--         (a, xs') <- p xs
--         let ln' = mapLine ln
--         let k0 = k -- if ln == ln' then (k) else (1)
--         let k' = k0 + length xs - length xs'
--         f xs' (ln', mapCol k', ((ln, (k0, k')), g a xs') : ys) -- g a xs' : ys)
--     lol mapCol xs g p ys = lol' mapCol id xs (\a _ -> g a) p ys

lx :: String -> Either String [(SrcRange, Tok String)]
lx s = fmap (\((_, _, q), _) -> reverse q) $ f s (1, 1, [])
    where
--     len = length s
    f    ('{':     xs) t = lol 1 xs (Pragma . lines)  (takeUntilC '}') t
    f    ('(':'*': xs) t = lol 2 xs (Comment . lines) (takeUntil "*)") t
    f xs@(' ':    _xs) t = lol 0 xs (Whitespace . (length)) (chars ' ') t
    f    ('\n':    xs) t = lol' 1 True xs (\_ _ -> NewLine) (const $ Right ((), xs)) t
    f    ('|':     xs) t = lol 1 xs (const VLine) (const $ Right ((), xs)) t
    f    ('+':     xs) t = lol 1 xs (const Cross) (const $ Right ((), xs)) t
    f    ('-':     xs) t = lol' 1 False xs (\a b -> HLine (length a) (countvl b)) (chars '-') t
    f    ('[':     xs) t = lol 1 xs (Contact) (takeUntil "]" ) t
    f    ('(':     xs) t = lol 1 xs (Coil)  (takeUntil ")" ) t
    f    ('>':     xs) t = lol 1 xs (const REdge) (const $ Right ((), xs)) t
    f    ('<':     xs) t = lol 1 xs (const FEdge) (const $ Right ((), xs)) t
    f    (':':     xs) t = lol 1 xs (const Colon) (const $ Right ((), xs)) t
    f xs@(c  :     _)  t
        | isDigit c      = lol 0 xs (Number . read)  (digits) t
        | isAlpha c || c=='%'     = lol 0 xs (Name)  (alphaNum) t
    f    []            t = Right (t, [])
    f (other:       _) _ = Left ("unexpected char '" ++ [other] ++ "'")

--     lol :: String -> (w -> Tok String)
--         -> (String -> Either String (w, String))
--         -> (Int, Int, [((Int, (Int, Int)), Tok String)])
--         -> Either String
--             ((Int, Int, [((Int, (Int, Int)), Tok String)]), String)
    lol' :: Int -> Bool -> String -> (w -> String -> Tok String)
        -> (String -> Either String (w, String))
        -> (Int, Int, [(SrcRange, Tok String)])
        -> Either String
            ((Int, Int, [(SrcRange, Tok String)]), String)
    lol' kk q xs g p (ln, k, ys) = do
        (a, xs') <- p xs
        case q of
             False -> do
                let k' = kk + k + length xs - length xs' - 1
                f xs' (ln, k' +1, ((ln, (k, k')), g a xs') : ys)
             True -> do
                let k' = k + length xs - length xs'
                f xs' (ln+1, 1, ((ln, (k, k')), g a xs') : ys)

    lol kk xs g p ys = lol' kk False xs (\a _ -> g a) p ys

    countvl xs = length $ takeWhile (=='|') xs
    digits cs = Right $ span (isDigit) cs
    alphaNum cs = Right $ span (\c -> isAlphaNum c || c == '_' || c == '%') cs
    chars c cs = Right $ span (==c) cs -- or until, not sure
    takeUntil n h = case T.breakOn (T.pack n) (T.pack h) of
                     (p, xs) -> case T.stripPrefix (T.pack n) xs of
                                     Nothing -> Left "wtf"
                                     Just xxs -> Right (T.unpack p, T.unpack xxs)
    takeUntilC n h = case break (==n) (h) of
                     (p, n':xs) | n'==n, isSuffixOf "\\" p
                        -> first ((p++[n'])++) <$> takeUntilC n xs
                     (p, n':xs) | n'==n -> Right (p, xs)
                     (_p, _xs) -> Left "lol"


lxx :: [(SrcRange, Tok String)] -> [(SrcRange, Tok String)]
lxx = f
    where
    f ((a, Name lbl) : (b, Colon) : xs) = (ext a b, Label lbl) : f xs
    f ((a, Number n) : (b, Colon) : xs) = (ext a b, Label (show n)) : f xs --FIXME jump to number
    f ((a, REdge) : (_, REdge) : (b, Name lbl) : xs) = (ext a b, Jump' lbl) : f xs
    f ((a, REdge) : (b, Name lbl) : (_, REdge) : xs) = (ext a b, Continuation lbl) : f xs
    f ((a, FEdge) : (_, Name ret) : (b, REdge) : xs)
        | fmap toLower ret == "return" = (ext a b, Return) : f xs
    f (x:xs) = x : f xs
    f [] = []
    ext (x, (a, _)) (_, (_, b))  = (x, (a, b))

--------------------------------------------------------------------------------

newtype ParseErr = PE String
    deriving (Show, Eq, Ord)

instance ShowErrorComponent ParseErr where
    showErrorComponent (PE e)  = show e

--------------------------------------------------------------------------------

withPos :: (Stream t, Ord e) => Parsec e t a -> Parsec e t ((SrcPos, SrcPos), a)
withPos p = (\a b c -> ((a, c), b)) <$> src <*> p <*> src
    where
    src = getSourcePos >>= \pos -> pure (unPos$sourceLine pos, unPos$sourceColumn pos)

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

lexerP :: Parsec ParseErr Text [((SrcPos, SrcPos), Tok Text)]
-- lexerP = space *> many (withPos lexeme <* space) <* eof
lexerP = many (withPos lexeme) <* eof

lexerLinesP :: Parsec ParseErr Text [[((SrcPos, SrcPos), Tok Text)]]
lexerLinesP = (fmap snd . breakLines) <$> lexerP

--------------------------------------------------------------------------------

type SrcPos = (Int, Int)

-- runLexer
--     :: Text
--     -> Either Text [ [((SrcPos, SrcPos), Tok Text)] ]
-- runLexer
--     = bimap (T.pack . errorBundlePretty) id
--     . parse lexerLinesP "(file)"
-- 
-- runLexer'
--     :: Text
--     -> Either String [ [((SrcPos, SrcPos), Tok Text)] ]
-- runLexer'
--     = first errorBundlePretty
--     . parse lexerLinesP "(file)"

--------------------------------------------------------------------------------

runLexer
    :: Text
    -> Either Text [ [(SrcRange, Tok Text)] ]
runLexer
    = first T.pack . runLexer'
--     = bimap (T.pack . errorBundlePretty) stripPos3
--     . parse lexerLinesP "(file)"

runLexer'
    :: Text
    -> Either String [ [(SrcRange, Tok Text)] ]
runLexer'
    = runLexer''
--     = bimap errorBundlePretty stripPos3
--     . parse lexerLinesP "(file)"


runLexer''
    :: Text
    -> Either String [ [(SrcRange, Tok Text)] ]
runLexer'' s
    = ((split ((NewLine==).snd)) . fmap (fmap (fmap T.pack)) . lxx) <$> (lx $ T.unpack s)

    where
        split p xs = case break p xs of
                          (a, []) -> [a]
                          (a, b : rest) -> (a ++ [b]) : split p rest
--                           _ -> undefined

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
    :: [((SrcPos, SrcPos), tok)]
    -> [(SrcPos, [((SrcPos, SrcPos), tok)])]
breakLines (x@((p, _), _) : xs) = (p, x : a) : breakLines b
    where
    (a, b) = span ((srcLine p==).srcLine.fst.fst) xs
    srcLine = fst
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

-- stripPos3
--     :: [[((SourcePos, SourcePos), a)]]
--     -> [[((Int, (Int, Int)), a)]]
-- stripPos3 = fmap (fmap (first line))
--     where
--     line (s, e) = (unPos$sourceLine s, (unPos$sourceColumn s, (unPos$sourceColumn e)-1))

stripPos3
    :: [[((SrcPos, SrcPos), a)]]
    -> [[(SrcRange, a)]]
stripPos3 = fmap (fmap (first line))
    where
    line (s, e) = (pline s, (column s, (column e)-1))
    column = snd
    pline = fst
-- fmap (fmap (first line))
--     where
--     line (s, e) = (unPos$sourceLine s, (unPos$sourceColumn s, (unPos$sourceColumn e)-1))

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

-- -- |Discard position informations from list of lexemes
-- dropPos
--     :: [[(p, Tok a)]]
--     -> [Tok a]
-- dropPos = foldMap (fmap snd)
-- 
-- dropPos2
--     :: [[(p, Tok a)]]
--     -> [[Tok a]]
-- dropPos2 = fmap (fmap snd)

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
