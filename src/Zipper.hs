{-# LANGUAGE OverloadedStrings #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Zipper where

import Prelude hiding (fail)
-- import System.Environment (getArgs)
-- import qualified Data.Text.IO as TIO
-- import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)
import Data.Traversable
import Data.Foldable
import Data.Text (Text, unpack)
import Data.Bifunctor
import Data.Maybe

import Ladder.LadderParser

import Ladder.Lexer

import Control.Monad hiding (fail)
-- import Debug.Trace
-- import GHC.Stack
-- import GHC.Exts

import Ladder.Zipper

--------------------------------------------------------------------------------

-- |Diagram parser input stream (or, say, input vortex)
type Dg a = Zp (Int, Zp ((Int, Int), a))

-- |Token position and extent
type DgExt = (Int, (Int, Int))

-- |Returns number of remaining tokens
dgLength :: Dg a -> Int
dgLength (Zp l r) = sum (fmap (zpLength.snd) l) + sum (fmap (zpLength.snd) r)

dgNull :: Dg a -> Bool
dgNull = (>0) . dgLength --FIXME

-- |Drop empty lines
dgTrim :: Dg a -> Dg a
dgTrim (Zp l r) = Zp (trim l) (trim r)
    where trim = filter (not . zpNull . snd)

--------------------------------------------------------------------------------

--XXX seem too general to have such specific name, 'StateAndFailureMonad' maybe?
newtype DgP a = DgP { dgp :: DgPSt -> Either String (a, DgPSt) }

-- |Parser state
data DgPSt = DgPSt
    { psNext     :: MoveToNext (Tok Text) -- ^select next token
    , psStr      :: Dg (Tok Text) -- ^input
    , psLastBite :: Maybe DgExt -- ^position of last token eaten
    , psFocused  :: Bool -- ^current focus of zp is actual parser current token
    }

instance Functor DgP where
    fmap = ap . return

instance Applicative DgP where
    pure = return
    (<*>) = ap

instance Monad DgP where
    return a = DgP $ \s -> return (a, s)
    a >>= b = DgP $ \s -> do
        (y, s') <- dgp a s
        dgp (b y) s'

instance MonadFail DgP where
    fail = DgP . const . Left

instance Alternative DgP where
    empty = DgP $ const $ Left "alt empty"
    a <|> b = DgP $ \s -> dgp a s <|> dgp b s

--------------------------------------------------------------------------------

-- |Move in some direction from provided origin
type MoveToNext tok = (Int, (Int, Int)) -> Dg tok -> Either String (Dg tok)
type Next = MoveToNext (Tok Text)

move_ :: Int -> Int -> Dg a -> Either String (Dg a)
move_ ln co dg = maybe (Left here) return (move ln co dg)
-- move_ ln co dg = maybe (Left here) return (moveNotCursed ln co dg)

--FIXME should move only to direct neigbour
-- (by using single step to exact position it probably works already)
goRight, goDown, goUp, goLeft :: Next
goRight (ln, (_, co)) = move_ ln     (co+1)
goDown  (ln, (co, _)) = move_ (ln+1) co
goUp    (ln, (co, _)) = move_ (ln-1) co
goLeft  (ln, (co, _)) = move_ ln     (co-1)

--------------------------------------------------------------------------------

lastPos :: DgP DgExt
lastPos = psLastBite <$> get >>= maybe (fail here) return

applyDgp :: DgP a -> Dg (Tok Text) -> Either String (a, DgPSt)
applyDgp p dg = dgp p (DgPSt goRight dg Nothing True)

get :: DgP DgPSt
get = DgP $ \s -> return (s, s)

put :: DgPSt -> DgP ()
put s = DgP $ \_ -> return ((), s)

modify :: (DgPSt -> DgPSt) -> DgP ()
modify f = f <$> get >>= put

setDir :: Next -> DgP ()
setDir f = modify $ \(DgPSt _ zp ps fc) -> DgPSt f zp ps fc

getDir :: DgP Next
getDir = psNext <$> get

step :: DgP ()
step = do
    origin             <- currentPos
    DgPSt f zp ps True <- get --if nothing is focused, currentPos makes no sense
    case f origin zp of
        Right zp' -> put (DgPSt f zp' ps True)
        Left err -> fail here --or not?

setPos :: (Int, (Int, b)) -> DgP ()
setPos (ln, (co, _)) = do
    DgPSt b zp ps _ <- get
    Just zp'        <- return $ move ln co zp --FIXME can only move to direct neighbour!!!!!!!
    put (DgPSt b zp' ps True)

--------------------------------------------------------------------------------

-- |Fail if input stream is not empty
dgIsEmpty :: DgP ()
dgIsEmpty
--     = (dgLength . psStr) <$> get
--     >>= \case
--         0 -> return ()
--         _ -> fail $ here ++ "not empty"
    =   (dgNull . psStr) <$> get
    >>= flip when (fail $ here ++ "not empty")

labelOnTop
    :: DgP a -- ^Device parser e.g. "(S)"
    -> DgP (Text, a)
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
withAbove :: DgP a -> DgP b -> DgP (a, b)
withAbove a b = undefined

operand = ((Var . unpack) <$> name)
    <|> (do
            Number n <- eat
            return $ Lit n)

withOperands
    :: DgP (Bool, a) -- ^Device parser e.g. "(S)", flag indicates presend of second operand
    -> DgP (Operand, Maybe Operand, a)
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

labelOnTop' :: DgP a -> DgP (String, a)
labelOnTop' p = bimap unpack id <$> labelOnTop p

{-
   |
   +-------
   |\
   | *-------
  
-}

branch
    :: ((Tok Text) -> Bool)
    -> [(Next, DgP a)]
    ->  DgP [a]
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
eol :: DgP ()
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

-- device :: DgP String -> DgP (Cofree (Diagram String Operand String) DgExt)
-- device p = do
--     pos <- currentPos
--     (lbl, f) <- labelOnTop' p
--     (pos :<) <$> (Device f [lbl] <$> hline'2)
-- 
-- coil2 :: DgP (Cofree (Diagram String Operand String) DgExt)
-- coil2 = device $ do
--     Coil f <- eat
--     return $ "(" ++ unpack f ++ ")"
-- 
-- contact2 :: DgP (Cofree (Diagram String Operand String) DgExt)
-- contact2 = device $ do
--     Contact f <- eat
--     return $ "[" ++ unpack f ++ "]"
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

-- |Succeeds FIXME FIXME i am not sure when
end :: DgP ()
end = do
    Nothing <- (cursor . psStr) <$> get
    return ()

eat :: DgP (Tok Text)
eat = do
    DgPSt nx dg ps True <- get
    case dgPop dg of
        Just (v, pos, dg') -> do
            put $ case nx pos dg' of
                Right q -> DgPSt nx q   (Just pos) True
                Left  _ -> DgPSt nx dg' (Just pos) False --nowhere to move
            return v
        Nothing -> fail $ show (here, ps)

currentPosM :: DgP (Maybe DgExt)
currentPosM = (pos . psStr) <$> get

currentPos :: DgP DgExt --(Int, (Int, Int))
currentPos = do
    Just p <- (pos . psStr) <$> get
    return p

peek :: DgP (Tok Text)
peek = do
    Just p <- (cursor . psStr) <$> get
    return p

--------------------------------------------------------------------------------

-- |Pop focused item, its extent and updated zipper
dgPop :: Dg a -> Maybe (a, DgExt, Dg a)
dgPop (Zp u ((ln, Zp l ((col, x) : rs)) : ds))
    = Just (x, (ln, col), Zp u ((ln, Zp l rs) : ds))
dgPop _ = Nothing

pattern DgFocused x <- Zp _ ((_, Zp _ ((_, x) : _)) : _)
-- pattern DgH' ln cl x <- Zp _ ((ln, Zp _ ((_, x) : _)) : _)
--pattern DgM = (Zp u ((ln, Zp l ((_, x) : rs)) : ds))

cursor :: Dg a -> Maybe a
cursor (DgFocused x) = Just x
cursor _             = Nothing

-- |Match on current token position
pattern DgFocusedPos ln cl cr <- Zp _ ((ln, Zp _ (((cl, cr), x) : _)) : _)

pos :: Dg a -> Maybe (Int, (Int, Int))
pos (DgFocusedPos ln cl cr) = Just (ln, (cl, cr))
pos _                       = Nothing

mkDgZp :: [(Int, [((Int, Int), (Tok Text))])] -> Dg (Tok Text)
mkDgZp = Zp [] . fmap (fmap (Zp []))

--------------------------------------------------------------------------------

-- moveNotCursed :: Int -> Int -> Dg a -> Maybe (Dg a)
-- moveNotCursed line col = moveToLine line >=> moveToCol col

move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col = (moveToLine line >=> moveToCol col) . focusDg

pattern DgLine us ln zp ds = Zp us ((ln, zp) : ds)

--FIXME merge with moveToLine somehow?
moveToCol :: Int -> Dg a -> Maybe (Dg a)
-- moveToCol col (Zp us ((ln, zp@(Zp l (((cl, cr), _) : _))) : ds))
-- moveToCol col (DgLine us ln zp@(Zp l (((cl, cr), _) : _)) ds)
--     | col >= cl = reassemble <$> moveTo stepRight (isIn . fst) zp
--     | otherwise = reassemble <$> moveTo stepLeft (isIn . fst) zp
--     where
--     isIn (a, b) = b >= col && a <= col
-- --     reassemble zp' = Zp us ((ln, zp') : ds)
--     reassemble zp' = DgLine us ln zp' ds
-- moveToCol _ _ = Nothing
moveToCol col (DgLine us ln zp ds) = reassemble <$> move2 (dir col . fst) zp
    where
    dir x (a, b)
        | x < a = LT
        | x > b = GT
        | otherwise = EQ
    reassemble zp' = DgLine us ln zp' ds
moveToCol _ _ = Nothing

focusDg :: Dg a -> Dg a
focusDg = fmap (fmap focus) . focus

moveToLine :: Int -> Dg a -> Maybe (Dg a)
moveToLine ln = move2 (compare ln . fst)
-- moveToLine' line zp@(Zp _ ( (ln, _) : _))
--     | line >= ln = moveTo stepRight ((line==).fst) zp
--     | otherwise = moveTo stepLeft ((line==).fst) zp
-- moveToLine' _ _ = Nothing

--------------------------------------------------------------------------------
