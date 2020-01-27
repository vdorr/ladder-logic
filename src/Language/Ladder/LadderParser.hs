#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.LadderParser
    ( Diagram(..)
    , mapDgA, mapDg
    , Operand(..)
    , DevType(..)
    , DevOpFlag(..)
    , LdP
    , runParser
    , ladder
    , ladderLiberal
-- *for testing only
    , box001
    ) where

import Prelude hiding (fail)
-- import Control.Monad.Fail
import Control.Applicative --hiding (fail)
import Control.Monad hiding (fail)
import Data.Foldable
import Data.Functor.Identity
import Data.Void
import Data.Function

import Control.Monad.State hiding (fail)

import Language.Ladder.Utils
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser

--------------------------------------------------------------------------------

-- |Ladder AST type
data Diagram continuation device label a
    = Source !a   -- ^start of power rail
    | Sink       -- ^where wire connects to (implied) right rail or intersect node
    | End        -- ^where vertical left rail ends at the bottom
--     | Stub       -- ^intersection of hline and node
    | Device !device !a
    | Jump   !label
    | Node   ![a] --order matters here
    | Cont   !continuation !a
    | Conn   !continuation
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
    deriving (Show, Eq, Functor)

data DevType t
    = Coil_    !t
    | Contact_ !t
    deriving (Show, Eq, Functor)

--------------------------------------------------------------------------------

data LdPCtx m text device = LdPCtx
    {
--         ctxHasSndOp :: DevType text -> m DevOpFlag
      ctxMkDev :: !(DevType text -> m (DevOpFlag, [Operand text] -> m device))
    }
    -- '+' node positions, maybe

--------------------------------------------------------------------------------

data DevOpFlag = None | Optional | Mandatory
--     deriving (Show, Eq)

type LdP device text
    = SFM
        (DgPState
            (LdPCtx (Either String) text device)
            (Tok text)
        )

type DeviceParser t d = DevType t
        -> Either
                String
                (DevOpFlag, [Operand t] -> Either String d)

runParser
    :: DeviceParser t d
    -> LdP d t a
    -> [[(DgExt, Tok t)]]
    -> Either String (a, Dg (Tok t))
runParser mkDev p s
    = (psStr <$>) <$> applyDgp p (mkDgZp (dropWhitespace2 s)) (LdPCtx mkDev)

runParserX
    :: DeviceParser t d
    -> LdP d t a
    -> [(DgExt, Tok t)]
    -> Either String (a, [(Maybe t, Dg (Tok t))])
runParserX mkDev p lxs = do --TODO
--     let blocks = labeledRungs lxs

    Left "TODO"
--accepts untreated (not split to line, with all whitespce) token stream
--handles labels

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

ladder :: LdP d t (Cofree (Diagram Void d t) DgExt)
ladder
    = setDir goDown
    *> (ann <$> currentPos <*> fmap Source vline')
    <* dgIsEmpty

-- like 'ladder' but do not check if all lexemes consumed
ladderLiberal :: LdP d t (Cofree (Diagram Void d t) DgExt)
ladderLiberal
    = setDir goDown
    *> (ann <$> currentPos <*> fmap Source vline')

--------------------------------------------------------------------------------

variable :: LdP d t (Operand t)
variable = Var <$> name

number :: LdP d t (Operand t)
number = eat >>= \case
    Number n -> return (Lit n)
    _ -> lift $ Left "expected number"

operand :: LdP d t (Operand t)
operand = variable <|> number

--------------------------------------------------------------------------------

-- withOperands2 :: LdP d t (DevOpFlag, a)
--               -> LdP d t (Operand t, Maybe (Operand t), a)
-- withOperands2 p = below (above_ p operand) optOper
--     where
--     optOper ((Mandatory, a), op) = (op,,a) <$> fmap Just operand
--     optOper ((Optional , a), op) = (op,,a) <$> option operand
--     optOper ((None     , a), op) = return (op, Nothing, a)


withOperands :: LdP d t (DevOpFlag, a)
              -> LdP d t ([Operand t], a)
-- withOperands3 p = below (above_ p operand) optOper
withOperands deviceParser
        =        operand
        `above'` deviceParser
        `below`  optOper
    where
    above' = flip above_
    optOper ((Mandatory, a), op) = ((,a).(op:)) <$> (pure <$> operand)
    optOper ((Optional , a), op) = ((,a).(op:)) <$> (toList <$> optional operand)
    optOper ((None     , a), op) = ((,a).(op:)) <$> pure []

-- withOperands
--     :: LdP d t (Bool, a) -- ^Device parser e.g. "(S)", flag indicates presence of second operand
--     -> LdP d t (Operand t, Maybe (Operand t), a)
-- withOperands p = below (above_ p (variable)) optOper
--     where
--     optOper ((True, a), op) = (op,,a) <$> fmap (Just . fmap id) operand
--     optOper ((_   , a), op) = return (op, Nothing, a)

--------------------------------------------------------------------------------

hline :: LdP d t ()
hline = eat >>= \case
    HLine _ _ -> return ()
    _ -> lift $ Left "expected horizontal line"
    
vline :: LdP d t ()
vline = eat >>= \case
    VLine -> return ()
    _ -> lift $ Left "expected vertical line"

--------------------------------------------------------------------------------

isTok :: (Functor f, Eq (f ())) => f a -> f a -> Bool
isTok = on (==) (fmap (const ()))

node :: LdP d t (Cofree (Diagram c d t) DgExt)
node = ann <$> currentPos <*> (Node <$> node')

node' :: LdP d t [Cofree (Diagram c d t) DgExt]
node'
    = branch
        (isTok Cross)
        [ (goRight, hline') --currentPosM>>=traceShowM>>
        , (goDown , vline')
        ]

--FIXME with 'node' may end only left rail, vline stemming from node must lead to another node
vline' :: LdP d t (Cofree (Diagram c d t) DgExt)
vline' = many vline *> (end2 <|> node)

end2 :: LdP d t (Cofree (Diagram c d t) DgExt)
end2 = end *> ((:< End) <$> colUnder <$> lastPos)

eol2 :: LdP d t (Cofree (Diagram c d t) DgExt)
eol2 = eol *> ((:< Sink) <$> colRight <$> lastPos)

gap2 :: LdP d t (Cofree (Diagram c d t) DgExt)
gap2 = gap *> ((:< Sink) <$> colRight <$> lastPos)

hline' :: LdP d t (Cofree (Diagram c d t) DgExt)
hline'
--     = some (hline2 <* option crossing)
--     *> (device <|> node <|> jump <|> eol2)
--     where
--     crossing = skipSome (isTok VLine) *> hline2
    = some hline2
    *> (device <|> node <|> jump <|> gap2 <|> eol2)

jump :: LdP d t (Cofree (Diagram c d t) DgExt)
jump = do
    p <- currentPos
    eat >>= \case
        Jump' lbl -> return $ p :< Jump lbl
        _ -> lift $ Left "expected jump"
    
-- "-||`EOL`" is not allowed
hline2 :: LdP d t ()
hline2 = do
    vl <- eat >>= \case
        HLine _ vl -> return vl
        _          -> lift $ Left "expected horizontal line"

--XXX replicateM_ vl (skip' (==VLine)) ?? fail if VLine already eaten
    when (vl > 0) $ do
        (ln, (co, _)) <- currentPos
        setPos (ln, (co + vl, ()))--TODO TEST move to same location is noop


device :: LdP d t (Cofree (Diagram c d t) DgExt)
device = do
    p        <- currentPos
    usr      <- psUser <$> get
    (ops, f) <-
        withOperands do
            dev <- coil' <|> contact'
            case ctxMkDev usr dev of
                 Left  _             -> lift $ Left here
                 Right (flag, mkDev) -> return (flag, mkDev)
    dev' <- case f ops of
         Left  _ -> lift $ Left here
         Right d ->  return d
    (p :<) <$> (Device dev' <$> hline')


coil' :: LdP d t (DevType t)
coil' = eat >>= \case
        Coil f -> return (Coil_ f)
        _ -> lift $ Left "expected coil"

contact' :: LdP d t (DevType t)
contact' = eat >>= \case
        Contact f -> return (Contact_ f)
        _ -> lift $ Left "expected contact"

-- device :: LdP (Dev t) t (DevOpFlag, DevType t)
--        -> LdP (Dev t) t (Cofree (Diagram c (Dev t) t) DgExt)
-- device p = do
--     pos <- currentPos
--     (op, op2, f) <- withOperands2 p
--     (pos :<) <$> (Device (Dev f (op : toList op2)) <$> hline')
-- 
-- coil :: LdP (Dev t) t (Cofree (Diagram c (Dev t) t) DgExt)
-- coil = device $ do
--     Coil f <- eat
--     return (None, Coil_ f)
-- 
-- contact :: LdP (Dev t) t (Cofree (Diagram c (Dev t) t) DgExt)
-- contact = device $ do
--     Contact f <- eat
--     LdPCtx{..} <- psUser <$> get
--     Right flag <- return $ ctxHasSndOp (Contact_ f)
--     return (flag, Contact_ f)

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

cross :: LdP d t ()
cross = eat >>= \case
        Cross -> return ()
        _ -> lift $ Left "expected '+'"

edge :: LdP d t (Tok t)
edge
    = eat >>= \t -> case t of
        REdge -> return t
        FEdge -> return t
        _ -> lift $ Left here --SFM $ \_ -> Left here

name :: LdP d t t
name = eat >>= \case
        Name lbl -> return lbl
        _ -> lift $ Left "expected name"

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

lwall :: LdP d t ()
lwall = vline <|> void edge

-- negIn = do
--     NegIn <- eat
--     return ()

--TODO check clearance
box :: LdP d t ()
box = do
--     (ln, (_, co)) <- currentPos

--     traceShowM (here, ln, co, "------------->")
--     Zp zpl zpr <- psStr <$> get
--     forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

    setDir goUp
--     VLine <- eat
--     currentPosM >>= (traceShowM . (here, "left wall", ))
    void $ some lwall -- <|> negIn
--     currentPosM >>= (traceShowM . (here, "left top corner",))
    setDir goRight
    cross
--     currentPosM >>= (traceShowM . (here,"top wall",))
    hline

    setDir goDown --parsing right side, look for output line position
--     currentPosM >>= (traceShowM . (here,"right top corner",))
    cross

--     currentPosM >>= (traceShowM . (here,"right wall",))
    --TODO parse box instance name
    void $ some do
--         (ln, co) <- currentPos
        vline
--         setPos (ln, co+1)
--         ??? peek & record position

--     currentPosM >>= (traceShowM . (here,"bottom right corner",))
    setDir goLeft
    cross

--     currentPosM >>= (traceShowM . (here,"bottom wall",))

--     Zp zpl zpr <- psStr <$> get
--     forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)


    hline


--     currentPosM >>= (traceShowM . (here,))
--     Zp zpl zpr <- psStr <$> get
--     forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

--     currentPosM >>= (traceShowM . (here,"bottom left corner",))
    setDir goUp
    cross

--     currentPosM >>= (traceShowM . (here,"remaining left wall",))
    void $ many lwall --0 or more

    return ()

--------------------------------------------------------------------------------
