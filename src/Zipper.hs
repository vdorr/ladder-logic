{-# LANGUAGE OverloadedStrings #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Zipper where

import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)
import Data.Traversable
import Data.Foldable
import Data.Text (Text, unpack)
import Data.Bifunctor
import Data.Maybe
import Control.Monad hiding (fail)

-- import Debug.Trace
-- import GHC.Stack
-- import GHC.Exts

import Ladder.Zipper
import Ladder.Lexer
import Ladder.DiagramParser
import Ladder.LadderParser

--------------------------------------------------------------------------------

type DgP = SFM DgPSt
type DgPSt = DgPState (Tok Text)
type Next = MoveToNext (Tok Text)

--------------------------------------------------------------------------------

-- |Fail if input stream is not empty
dgIsEmpty :: SFM (DgPState tok) ()
dgIsEmpty
    =   (dgNull . psStr) <$> get
    >>= flip when (fail $ here ++ "not empty")

labelOnTop
    :: SFM (DgPState (Tok txt)) a -- ^Device parser e.g. "(S)"
    -> SFM (DgPState (Tok txt)) (txt, a)
labelOnTop p = do
    (ln, co) <- currentPos
    x        <- p
    next     <- currentPos
    setPos (ln-1, co)
    lbl      <- name
    setPos next
    return (lbl, x)

--TODO
--TODO
--TODO
--vertical combinators
-- p `withAbove` name
--   `withBelow` (optional name)
--TODO
--TODO
--TODO

-- |If a succeeds, parse also b positioned above first char of a
withAbove :: SFM (DgPState tok) a -> SFM (DgPState tok) b -> SFM (DgPState tok) (a, b)
withAbove a b = undefined

operand = ((Var . unpack) <$> name)
    <|> (do
            Number n <- eat
            return $ Lit n)

withOperands
    :: SFM (DgPState (Tok Text)) (Bool, a) -- ^Device parser e.g. "(S)", flag indicates presend of second operand
    -> SFM (DgPState (Tok Text)) (Operand, Maybe Operand, a)
withOperands p = do
    (ln, co) <- currentPos
    (b, x)   <- p
    next     <- currentPos
    setPos (ln - 1, co)
    op       <- (Var . unpack) <$> name
    op2 <- if b
        then do
            setPos (ln + 1, co)
            Just <$> operand
        else
            return Nothing
    setPos next
    return (op, op2, x)

labelOnTop' :: SFM (DgPState (Tok Text)) a -> SFM (DgPState (Tok Text)) (String, a)
labelOnTop' p = bimap unpack id <$> labelOnTop p

{-
   |
   +-------
   |\
   | *-------
  
-}

branch
    :: (tok -> Bool)
    -> [(MoveToNext tok, SFM (DgPState tok) a)]
    ->  SFM (DgPState tok) [a]
branch isFork branches = do
    origin <- currentPos
    True   <- isFork <$> peek
    stuff  <- for branches $ \(dir, p) -> do
        setDir dir
        (setPos origin *> step *> (Just <$> p))
        <|> return Nothing --step fail if there's nothing in desired direction
    setPos origin --eat `fork`
--     setDir dir0 --restore direction, good for parsing boxes
    eat --FIXME set direction!!!!!!!!!!!!!
    return $ catMaybes stuff

-- branch'
--     :: ((Tok Text) -> Maybe b)
--     -> [(Next, DgP a)]
--     ->  DgP (b, [a])
-- branch' isFork branches = do
--     origin <- currentPos
--     dir0 <- psNext <$> get
--     Just f <- isFork <$> peek_
--     stuff <- for branches $ \(dir, p) -> do
--         setDir dir
--         setPos origin
--         step --with dir
--         p
--     setPos origin --eat `fork`
--     setDir dir0 --restore direction, good for parsing boxes
--     eat
--     return (f, stuff)

-- |Matches diagram with nothing remaining on current line
pattern DgLineEnd <- Zp _l ((_ln, Zp _ []) : _)

-- |Succeeds only when positioned on end of line
eol :: SFM (DgPState tok) ()
eol = do
    psStr <$> get >>= \case
        DgLineEnd -> return ()
        _ -> fail here

colRight :: DgExt -> DgExt
colRight (ln, (_, co)) = (ln, (co + 1, co + 1))

colUnder :: DgExt -> DgExt
colUnder (ln, (_, co)) = (ln + 1, (co, co))

--------------------------------------------------------------------------------

hline = do
    HLine <- eat
    return ()

vline = do
    VLine <- eat
    return ()

--------------------------------------------------------------------------------
#if 0
--FIXME parse it with DgP's pos type and then fmap it to 'Pos'
currentPos2 :: DgP Pos
currentPos2 = fmap Pos $ fmap (fmap fst) currentPos

extToPos :: DgExt -> Pos
extToPos = (\(ln, co) -> Pos (co, ln)) . fmap fst
    
toPos2 :: Cofree (Diagram String Operand String) DgExt -> Cofree (Diagram String Operand String) Pos
toPos2 = fmap extToPos

-- test002 :: DgP (Cofree (Diagram String Operand String) Pos)
-- test002 = fmap extToPos <$> test002'
#endif
--------------------------------------------------------------------------------

{-
hlink   : '-'+ ('|'+ '-'+)?
hline   : (hlink | contact | coil | node)+
node    : '+'
name    : <cf tokenizer>

--interpret as 2D syntax
contact : name
          '[' contactType ']'
          name?

coil    : name
          '[' coilType ']'

contactType : ...
coilType    : ...

-}

data Operand
    = Var String
    | Lit Int
    deriving (Show, Eq)

test002' :: DgP (Cofree (Diagram String Operand String) DgExt)
test002'
    = setDir goDown
    *> ((:<) <$> currentPos <*> fmap Source vline'2)
    <* dgIsEmpty

node2 :: DgP (Cofree (Diagram String Operand String) DgExt)
node2 = (:<) <$> currentPos <*> (Ladder.LadderParser.Node <$> node2')

node2' :: DgP [Cofree (Diagram String Operand String) DgExt]
node2'
    = branch
        (==Cross)
        [ (goRight, hline'2) --currentPosM>>=traceShowM>>
        , (goDown , vline'2)
        ]

--FIXME with 'node2' may end only left rail, vline stemming from node must lead to another node
vline'2 :: DgP (Cofree (Diagram String Operand String) DgExt)
vline'2 = many vline2 *> (end2 <|> node2)

end2 :: DgP (Cofree (Diagram String Operand String) DgExt)
end2 = end *> ((:< End) <$> colUnder <$> lastPos)

eol2 :: DgP (Cofree (Diagram String Operand String) DgExt)
eol2 = eol *> ((:< Sink) <$> colRight <$> lastPos)

hline'2 :: DgP (Cofree (Diagram String Operand String) DgExt)
hline'2 = some hline2 *> (coil2 <|> contact2 <|> node2 <|> eol2) --TODO vline crossing

vline2 :: DgP ()
vline2 = do
    VLine <- eat
    return ()

hline2 :: DgP () --TODO vline crossing
hline2 = do
    HLine <- eat
    return ()

device :: DgP (Bool, String) -> DgP (Cofree (Diagram String Operand String) DgExt)
device p = do
    pos <- currentPos
    (op, op2, f) <- withOperands p
    (pos :<) <$> (Device f (op : toList op2) <$> hline'2)

coil2 :: DgP (Cofree (Diagram String Operand String) DgExt)
coil2 = device $ do
    Coil f <- eat
    return (False, "(" <> unpack f <> ")")

-- contact2 :: DgP (Bool, Cofree (Diagram String Operand String) DgExt)
contact2 = device $ do
    Contact f <- eat
    return (elem f cmp, "[" <> unpack f <> "]")
    where
    cmp = [">", "<", "=", "==", "<>", "/=", "!=", "≠", "≤", "≥"]

--------------------------------------------------------------------------------

{-

--very approximate syntax

wall   : '+' '-'+ '+'

top    : name
         wall

bottom : wall


left   : '0'? ('|' | '>' | '<') name?

right  : name? '|' '0'?

-}

{-

smallest possible box:
    +-+
    | |
    +-+

clearance (example of incorrect box):
     |    // <--- reject!
   +-+
 --| |
   +-+--  // <--- reject!

-}

node = do
    Cross <- eat
    return ()

edge
    = eat >>= \t -> case t of
        REdge -> return t
        FEdge -> return t
        _ -> fail here

name = do
    Name lbl <- eat
    return lbl

-- -- |parse left side of a box
-- leftSide = do
--     --VLine, REdge, FEdge, Name, connections (HLine+Name)
--     some leftSideBrick
--     return ()
-- 
-- leftSideBrick = do
--     (ln, co) <- currentPos
-- --     vline <|> edge
--     branch
--         (\case
--             VLine -> True
--             REdge -> True
--             FEdge -> True
--             _ -> False)
--         [ (goLeft, hline *> name)
--         , (goRight, name)
--         ]
--     setDir goDown --i guess branch should restore direction
    
--     setPos (ln, co+1)
--     setDir goRight

box001 :: Int -> DgP ()
box001 ln = do
    setPos (ln, (1, 1))
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

lwall = vline <|> void edge

-- negIn = do
--     NegIn <- eat
--     return ()

--TODO check clearance
box = do
--     (ln, (_, co)) <- currentPos

--     traceShowM (here, ln, co, "------------->")
--     Zp zpl zpr <- psStr <$> get
--     forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

    setDir goUp
--     VLine <- eat
--     currentPosM >>= (traceShowM . (here, "left wall", ))
    some lwall -- <|> negIn
--     currentPosM >>= (traceShowM . (here, "left top corner",))
    setDir goRight
    node
--     currentPosM >>= (traceShowM . (here,"top wall",))
    hline

    setDir goDown --parsing right side, look for output line position
--     currentPosM >>= (traceShowM . (here,"right top corner",))
    node

--     currentPosM >>= (traceShowM . (here,"right wall",))
    --TODO parse box instance name
    some $ do
--         (ln, co) <- currentPos
        vline
--         setPos (ln, co+1)
--         ??? peek & record position

--     currentPosM >>= (traceShowM . (here,"bottom right corner",))
    setDir goLeft
    node

--     currentPosM >>= (traceShowM . (here,"bottom wall",))

--     Zp zpl zpr <- psStr <$> get
--     forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)


    hline
    
    
--     currentPosM >>= (traceShowM . (here,))
--     Zp zpl zpr <- psStr <$> get
--     forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

--     currentPosM >>= (traceShowM . (here,"bottom left corner",))
    setDir goUp
    node

--     currentPosM >>= (traceShowM . (here,"remaining left wall",))
    many lwall --0 or more

    return ()

--------------------------------------------------------------------------------
