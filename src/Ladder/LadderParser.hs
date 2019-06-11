{-# LANGUAGE OverloadedStrings #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Ladder.LadderParser where

import Data.Text (Text, unpack)
import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Applicative --hiding (fail)
import Control.Monad hiding (fail)
import Data.Bifunctor
import Data.Foldable

import Language.Ladder.Utils
import Ladder.Lexer
import Ladder.DiagramParser

import Debug.Trace

--------------------------------------------------------------------------------

data Diagram c d s a
    = Source a   -- ^start of power rail
    | Sink       -- ^where wire connects to (implied) right rail
    | End        -- ^where vertical left rail ends at the bottom
--     | Stub       -- ^intersection of hline and node
    | Device d a --
    | Jump   s
    | Node   [a] --order matters here
--     | Label s --i dislike it, but would need it if vline can cross line with label
    | Cont c a
    | Conn c
    deriving (Show, Functor, Eq, Foldable, Traversable)

mapDg :: (c -> c') -> (d -> d') -> (s -> s') -> Diagram c d s a -> Diagram c' d' s' a
mapDg z x y = f
    where
    f (Source   a) = Source       a
    f  Sink        = Sink
    f  End         = End
    f (Device d a) = Device (x d) a
    f (Jump s    ) = Jump   (y s)
    f (Node     a) = Node         a
    f (Conn c    ) = Conn   (z c)
    f (Cont c   a) = Cont   (z c) a
--     f  Stub        = Stub

data Operand
    = Var String
    | Lit Int
    deriving (Show, Eq)

data Dev = Dev String [Operand]
    deriving (Show, Eq)

--------------------------------------------------------------------------------

type DgP = SFM DgPSt
type DgPSt = DgPState (Tok Text)
type Next = MoveToNext (Tok Text)

--------------------------------------------------------------------------------

--TODO
--vertical combinators
#if 0
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

labelOnTop' :: SFM (DgPState (Tok Text)) a -> SFM (DgPState (Tok Text)) (String, a)
labelOnTop' p = bimap unpack id <$> labelOnTop p
#endif

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

variable :: DgP Operand
variable = (Var . unpack) <$> name

number :: DgP Int
number = do
    Number n <- eat
    return n

operand :: DgP Operand
operand = variable
    <|> (do
            Number n <- eat
            return $ Lit n)
#if 0
test p pp = do
    current <- currentPos
    x       <- p
    next    <- currentPos
    y       <- pp current x
    setPos next
    return (x, y)

test1 mapPos p pp = do
    begin <- currentPos
    x     <- p
    next  <- currentPos
    setPos (mapPos begin)
    y     <- pp x
    setPos next
    return (x, y)

test11 lbl mapPos p pp = do
    begin <- currentPos
    traceShowM (here, lbl, "--->>", begin)
    x     <- p
    traceShowM (here, lbl, begin)
    next  <- currentPos
    traceShowM (here, lbl, "setPos")
    qq <- setPosOrBlur (mapPos begin)
    traceShowM (here, lbl, "begin:", begin, "next:", next, "mapped:", mapPos begin, qq)
    y     <- pp x
    setPos next
    p <- currentPosM
    traceShowM (here, lbl, "<<---", p)
    return y


--TODO should be usable also for block labels
above_ p pp = test p $ \(ln, co) _ -> setPos (ln - 1, co) >> pp

below_ p pp = test p $ \(ln, co) _ -> setPos (ln + 1, co) >> pp

above p pp = test p $ \(ln, co) x -> setPos (ln - 1, co) >> pp x
below p pp = test p $ \(ln, co) x -> setPos (ln + 1, co) >> pp x

-- above1_ p pp = test1 (\(ln, co) -> (ln - 1, co)) p (const pp)
above1_ p pp = above1 p (const pp)
above1 = test1 (\(ln, co) -> (ln - 1, co))

below11 = test11 "below" (\(ln, co) -> (ln + 1, co))

above11_
    :: SFM (DgPState tok) a
    -> SFM (DgPState tok) b
    -> SFM (DgPState tok) (a, b)
above11_ p pp = test11 "above" (\(ln, co) -> (ln - 1, co))
    (p)
    (\x -> pp >>= \y -> return (x, y))

withOperands11 p
    = (below11
        (above11_ 
            (traceShowM here >> 
                (p) <* (traceShowM here))
            (currentPos >>= \pos -> traceShowM (here, pos) >> variable
                <* (currentPos >>= \pos -> traceShowM (here, pos))
                ))
        optOper)
    <* (currentPos >>= \pos -> traceShowM (here, pos))
    where
    optOper ((True, a), op) = undefined -- (op,,a) <$> fmap Just operand
    optOper ((_   , a), op) = traceShowM here >> return (op, Nothing, a)


withOperands1 p = below (above_ p variable) optionalOperand
    where
    optionalOperand (x, _) = Just <$> operand
    optionalOperand _      = return Nothing


-- withOperands2 :: _
withOperands2 p
    = (\(((_, a), op), mop) -> (op, mop, a))
    <$> below (above_ p variable) optOper
    where
    optOper ((True, _), _) = Just <$> operand
    optOper _              = return Nothing
#endif

withOperands
    :: SFM (DgPState (Tok Text)) (Bool, a) -- ^Device parser e.g. "(S)", flag indicates presence of second operand
    -> SFM (DgPState (Tok Text)) (Operand, Maybe Operand, a)
#if 0
withOperands = withOperands11
-- withOperands p = above11_ p variable >>= (\((_, b), a) -> return (a, Nothing, b) )
#else
withOperands p = do
    (ln, co) <- currentPos
    (b, x)   <- p
    next     <- currentPos
    setPos (ln - 1, co)
    op       <- variable
    op2 <- if b
        then do
            setPos (ln + 1, co)
            Just <$> operand
        else
            return Nothing
    setPos next
    return (op, op2, x)
#endif

--------------------------------------------------------------------------------

hline :: DgP ()
hline = do
    HLine _ _ <- eat
    return ()

vline :: DgP ()
vline = do
    VLine <- eat
    return ()

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

test002' :: DgP (Cofree (Diagram () Dev String) DgExt)
test002'
    = setDir goDown
    *> ((:<) <$> currentPos <*> fmap Source vline'2)
    <* dgIsEmpty

node2 :: DgP (Cofree (Diagram c Dev String) DgExt)
node2 = (:<) <$> currentPos <*> (Ladder.LadderParser.Node <$> node2')

node2' :: DgP [Cofree (Diagram c Dev String) DgExt]
node2'
    = branch
        (==Cross)
        [ (goRight, hline'2) --currentPosM>>=traceShowM>>
        , (goDown , vline'2)
        ]

--FIXME with 'node2' may end only left rail, vline stemming from node must lead to another node
vline'2 :: DgP (Cofree (Diagram c Dev String) DgExt)
vline'2 = many vline2 *> (end2 <|> node2)

end2 :: DgP (Cofree (Diagram c Dev String) DgExt)
end2 = end *> ((:< End) <$> colUnder <$> lastPos)

eol2 :: DgP (Cofree (Diagram c Dev String) DgExt)
eol2 = eol *> ((:< Sink) <$> colRight <$> lastPos)

hline'2 :: DgP (Cofree (Diagram c Dev String) DgExt)
hline'2
    = some (hline2 <* option crossing)
    *> (coil2 <|> contact2 <|> node2 <|> eol2)
    where
    crossing = skipSome (==VLine) *> hline2

vline2 :: DgP ()
vline2 = do
    VLine <- eat
    return ()

hline2 :: DgP () --TODO vline crossing
hline2 = do
    HLine _ vl <- eat
    when (vl > 0) $ do
        (ln, (co, _)) <- currentPos
        setPos (ln, (co + vl, ()))
    return ()
--TODO TEST move to same location is noop

device :: DgP (Bool, String) -> DgP (Cofree (Diagram c Dev String) DgExt)
device p = do
    pos <- currentPos
    (op, op2, f) <- withOperands p
    (pos :<) <$> (Device (Dev f (op : toList op2)) <$> hline'2)

coil2 :: DgP (Cofree (Diagram c Dev String) DgExt)
coil2 = do
--     traceShowM (here, "trying Coil")
    device $ do
        Coil f <- eat
        return (False, "(" <> unpack f <> ")")

contact2 :: DgP (Cofree (Diagram c Dev String) DgExt)
contact2 = do
--     traceShowM (here, "trying Contact")
    device $ do
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

edge :: DgP (Tok Text)
edge
    = eat >>= \t -> case t of
        REdge -> return t
        FEdge -> return t
        _ -> fail here --SFM $ \_ -> Left here

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
box :: DgP ()
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
