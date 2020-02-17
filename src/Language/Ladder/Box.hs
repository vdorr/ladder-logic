#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.Box where

import Control.Applicative --hiding (fail)
import Control.Monad
import Control.Monad.State

import Language.Ladder.Lexer
import Language.Ladder.LadderParser
import Language.Ladder.DiagramParser

--------------------------------------------------------------------------------

--TODO use box parser as just another device parser
box001 :: Int -> LdP d t ()
box001 ln = do
    setPos (ln, (1, ()))
    box

-- portName :: Int -> DgP a -> DgP (Text, a)
-- portName d p = do
--     (ln, co) <- currentPos
--     x <- p
--     next <- currentPos
--     setPos (ln, co+d)
--     lbl <- name
--     setPos next
--     return (lbl, x)

edge :: LdP d t (Tok t)
edge
    = eat >>= \t -> case t of
        REdge -> return t
        FEdge -> return t
        _ -> lift $ Left here

lwall :: LdP d t ()
lwall = vline <|> void edge

-- negIn = do
--     NegIn <- eat
--     return ()

--TODO check clearance
box :: LdP d t ()
box = do
    setDir goUp
    void $ some lwall -- <|> negIn
    setDir goRight
    cross
    void hline
    setDir goDown --parsing right side, look for output line position
    cross
    --TODO parse box instance name
    void (some vline)
    setDir goLeft
    cross
    void hline
    setDir goUp
    cross
    void $ many lwall --0 or more

    return ()

--------------------------------------------------------------------------------
