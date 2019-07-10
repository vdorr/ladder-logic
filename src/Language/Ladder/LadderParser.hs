{-# LANGUAGE OverloadedStrings #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.LadderParser
    ( Diagram(..)
    , mapDgA, mapDg
    , Operand(..)
    , Dev(..)
    , DevType(..)
    , ladder
    , parseLadderLiberal
    , runLadderParser, runLadderParser_
-- *for testing only
    , box001
    ) where

import Data.Text (Text, unpack)
import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Applicative --hiding (fail)
import Control.Monad hiding (fail)
import Data.Foldable
import Data.Functor.Identity
import Data.Void

import Language.Ladder.Utils
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser

--------------------------------------------------------------------------------

-- |Ladder AST type
data Diagram continuation device label a
    = Source a   -- ^start of power rail
    | Sink       -- ^where wire connects to (implied) right rail
    | End        -- ^where vertical left rail ends at the bottom
--     | Stub       -- ^intersection of hline and node
    | Device device a --
    | Jump   label
    | Node   [a] --order matters here
    | Cont   continuation a
    | Conn   continuation
    deriving (Show, Functor, Eq, Foldable, Traversable)

mapDg :: (c -> c') -> (d -> d') -> (s -> s') -> Diagram c d s a -> Diagram c' d' s' a
mapDg z x y = runIdentity . mapDgA (pure . z) (pure . x) (pure . y)

mapDgA
    :: Applicative f
    => (c -> f c')
    -> (d -> f d')
    -> (s -> f s')
    -> Diagram c d s a
    -> f (Diagram c' d' s' a)
mapDgA z x y = f
    where
    f (Source   a) = pure $ Source     a
    f  Sink        = pure $ Sink
    f  End         = pure $ End
    f (Device d a) =        Device <$> x d <*> pure a
    f (Jump s    ) =        Jump   <$> y s
    f (Node     a) = pure $ Node       a
    f (Conn c    ) =        Conn   <$> z c
    f (Cont c   a) =        Cont   <$> z c <*> pure a
--     f  Stub        = Stub

-- |Contact operand, located above or below
data Operand address
    = Var !address   -- ^name of memory location
    | Lit !Int       -- ^integer literal, usually allowed only below contact
    deriving (Show, Eq)

data DevType
    = Coil_    !String
    | Contact_ !String
    deriving (Show, Eq)

data Dev = Dev DevType [Operand String]
    deriving (Show, Eq)

--------------------------------------------------------------------------------

data LdPCtx text device = LdPCtx (text -> Maybe (Bool, device))
-- data LdPCtx text device = LdPCtx (text -> Maybe (Bool, [Operand text] -> device))

type DgP = SFM (DgPState (LdPCtx Text Dev) (Tok Text))
-- type DgP = SFM (DgPState (LdPCtx Text ) (Tok Text))
-- type DgPSt = DgPState (Tok Text)

type Ladder' device text = Cofree (Diagram Void device text) DgExt
type LdP device text a = SFM (DgPState (LdPCtx text device) (Tok text)) a
type Result1 device text = LdP device text (Ladder' device text)

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

ladder :: DgP (Cofree (Diagram Void Dev String) DgExt)
ladder
    = setDir goDown
    *> ((:<) <$> currentPos <*> fmap Source vline'2)
    <* dgIsEmpty

-- like 'ladder' but do not check if all lexemes consumed
parseLadderLiberal :: DgP (Cofree (Diagram (Void) Dev String) DgExt)
parseLadderLiberal
    = setDir goDown
    *> ((:<) <$> currentPos <*> fmap Source vline'2)

type Ladder = Cofree (Diagram Void Dev String) DgExt

-- applyDgp :: SFM (DgPState st tok) a -> Dg tok -> st -> Either String (a, DgPState st tok)
runLadderParser_ :: DgP a -> [(Int, [((Int, Int), Tok Text)])] -> Either String a
-- runLadderParser_ p s = fst <$> applyDgp p (mkDgZp (dropWhitespace s)) ()
runLadderParser_ p s = fst <$> runLadderParser p s

runLadderParser
    :: DgP a
    -> [(Int, [((Int, Int), Tok Text)])]
    -> Either String (a, Dg (Tok Text))
runLadderParser p s = (psStr <$>) <$> applyDgp p (mkDgZp (dropWhitespace s)) (LdPCtx undefined)

--------------------------------------------------------------------------------

variable :: DgP (Operand String)
variable = (Var . unpack) <$> name

number :: DgP (Operand String)
number = do
    Number n <- eat
    return (Lit n)

operand :: DgP (Operand String)
operand = variable <|> number

--------------------------------------------------------------------------------

withOperands
    :: DgP (Bool, a) -- ^Device parser e.g. "(S)", flag indicates presence of second operand
    -> DgP ((Operand String), Maybe (Operand String), a)
withOperands p = below (above_ p variable) optOper
    where
    optOper ((True, a), op) = (op,,a) <$> fmap Just operand
    optOper ((_   , a), op) = return (op, Nothing, a)
-- withOperands p = do
--     (ln, co) <- currentPos
--     (b, x)   <- p
--     next     <- currentPos
--     setPos (ln - 1, co)
--     op       <- variable
--     op2 <- if b
--         then do
--             setPos (ln + 1, co)
--             Just <$> operand
--         else
--             return Nothing
--     setPos next
--     return (op, op2, x)

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

node2 :: DgP (Cofree (Diagram c Dev String) DgExt)
node2 = (:<) <$> currentPos <*> (Node <$> node2')

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
    *> (coil2 <|> contact2 <|> node2 <|> jump <|> eol2)
    where
    crossing = skipSome (==VLine) *> hline2

jump :: DgP (Cofree (Diagram c Dev String) DgExt)
jump = do
    pos <- currentPos
    Jump' name <- eat
    return $ pos :< Jump (unpack name)

vline2 :: DgP ()
vline2 = do
    VLine <- eat
    return ()

hline2 :: DgP () --TODO vline crossing
hline2 = do
    HLine _ vl <- eat
--XXX replicateM_ vl ??
    when (vl > 0) $ do
        (ln, (co, _)) <- currentPos
        setPos (ln, (co + vl, ()))
--TODO TEST move to same location is noop

device :: DgP (Bool, DevType) -> DgP (Cofree (Diagram c Dev String) DgExt)
device p = do
    pos <- currentPos
    (op, op2, f) <- withOperands p
    (pos :<) <$> (Device (Dev f (op : toList op2)) <$> hline'2)

coil2 :: DgP (Cofree (Diagram c Dev String) DgExt)
coil2 = device $ do
    Coil f <- eat
    return (False, Coil_ (unpack f))

contact2 :: DgP (Cofree (Diagram c Dev String) DgExt)
contact2 = device $ do
    Contact f <- eat
    return (elem f cmp, Contact_ (unpack f))
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

node :: DgP ()
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
