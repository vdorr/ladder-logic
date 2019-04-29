{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances,
	PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable,
	LambdaCase, ScopedTypeVariables, ViewPatterns #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Zipper where

import Prelude hiding (fail)
-- import System.Environment (getArgs)
-- import qualified Data.Text.IO as TIO
-- import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)
import Data.Traversable
-- import Data.Foldable
import Data.Text (Text, unpack)
import Data.Bifunctor
import Data.Maybe

-- import Preprocess
import LadderParser hiding (node, hline, Node, device)
import qualified LadderParser
import DiagramParser (Pos(..))
import Tokenizer

import Control.Monad hiding (fail)
import Debug.Trace
import GHC.Stack
import GHC.Exts

--------------------------------------------------------------------------------

data Zp a = Zp [a] [a]
	deriving (Show, Functor, Eq) -- , Foldable)

zpFromList :: [a] -> Zp a
zpFromList = Zp []

zpToList :: Zp a -> [a]
zpToList (Zp l r) = reverse l ++ r

zpLength :: Zp a -> Int
zpLength (Zp l r) = length l + length r

zpNull :: Zp a -> Bool
zpNull = (<=0) . zpLength

-- |Bring something into focus
foc :: Zp a -> Zp a
foc (Zp (x:xs) []) = Zp xs [x]
foc zp = zp

tip :: Zp a -> Maybe a
tip (Zp _ (x:_)) = Just x
tip _ =  Nothing

pattern ZpR' x <- Zp _ (x : _)
pattern ZpR l f r = Zp l (f : r)
pattern ZpL l f r = Zp (f : l) r

stepLeft :: Zp a -> Maybe (Zp a)
stepLeft (ZpL l foc r) = Just (ZpR l foc r)
stepLeft _ = Nothing

stepRight :: Zp a -> Maybe (Zp a)
stepRight (ZpR l foc r) = Just (ZpL l foc r)
stepRight _ = Nothing

-- |Move to first element where predicate holds or fail
moveTo
	:: (Zp a -> Maybe (Zp a)) -- ^move function
	-> (a -> Bool) -- ^predicate
	-> Zp a
	-> Maybe (Zp a)
moveTo move test zp@(ZpR l foc r) -- = undefined
	| test foc = pure zp
	| otherwise = move zp >>= moveTo move test
moveTo _ _ _ = Nothing

move2 :: (a -> Ordering) -> Zp a -> Maybe (Zp a)
move2 f zp@(Zp _ (x : xs))
	| LT == f x   = moveTo stepLeft  ((==EQ).f) zp
	| otherwise   = moveTo stepRight ((==EQ).f) zp
move2 _ _ = Nothing

--------------------------------------------------------------------------------

-- |Diagram parser input stream (or, say, input vortex)
type Dg a = Zp (Int, Zp ((Int, Int), a))

-- |Token position and extent
type DgExt = (Int, (Int, Int))

-- |Returns number of remaining tokens
dgLength :: Dg a -> Int
dgLength (Zp l r) = sum (fmap (zpLength.snd) l) + sum (fmap (zpLength.snd) r)

-- type DgPSt = (Next, Dg Tok)
data DgPSt = DgPSt
	{ psNext :: Next
	, psStr :: Dg (Tok Text)
	, psLastBite :: Maybe DgExt -- ^position of last token eaten
	}

lastPos :: DgP DgExt
lastPos = psLastBite <$> get >>= maybe (fail here) return

applyDgp :: DgP a -> Dg (Tok Text) -> Either String (a, DgPSt)
applyDgp p dg = dgp p (DgPSt goRight dg Nothing)

newtype DgP a = DgP { dgp :: DgPSt -> Either String (a, DgPSt) }

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

get :: DgP DgPSt
get = DgP $ \s -> return (s, s)

put :: DgPSt -> DgP ()
put s = DgP $ \_ -> return ((), s)

modify :: (DgPSt -> DgPSt) -> DgP ()
modify f = f <$> get >>= put

-- |Drop empty lines
dgTrim :: Dg a -> Dg a
dgTrim (Zp l r) = Zp (filter (not.zpNull.snd) l) (filter (not.zpNull.snd) r)

--------------------------------------------------------------------------------

setDir :: Next -> DgP ()
setDir f = modify $ \(DgPSt _ zp ps) -> DgPSt f zp ps

getDir :: DgP Next
getDir = psNext <$> get

step :: DgP ()
step = do
	origin <- currentPos
	DgPSt f zp ps <- get
	case f origin zp of
		Right zp' -> put (DgPSt f zp' ps)
		Left err -> fail here --or not?

setPos :: (Int, (Int, b)) -> DgP ()
setPos (ln, (co, _)) = do
	DgPSt b zp ps <- get
	Just zp' <- return $ move ln co zp --FIXME can only move to direct neighbour!!!!!!!
	put (DgPSt b zp' ps)

--move in some direction from provided origin
type Next = (Int, (Int, Int)) -> Dg (Tok Text) -> Either String (Dg (Tok Text))

move_ :: Int -> Int -> Dg a -> Either String (Dg a)
move_ ln co dg = maybe (Left here) return (move ln co dg)

--FIXME should move only to direct neigbour
-- (by using single step to exact position it probably works already)
goRight, goDown, goUp, goLeft :: Next
goRight (ln, (_, co)) = move_ ln (co+1)
goDown (ln, (co, _)) = move_ (ln+1) co
goUp (ln, (co, _)) = move_ (ln-1) co
goLeft (ln, (co, _)) = move_ ln (co-1)

--------------------------------------------------------------------------------

-- |Fail if input stream is not empty
dgIsEmpty :: DgP ()
dgIsEmpty
	= (dgLength . psStr) <$> get
	>>= \case
		 0 -> return ()
		 _ -> fail $ here ++ "not empty"

labelOnTop :: DgP a -> DgP (Text, a)
labelOnTop p = do
	(ln, co) <- currentPos
	x <- p
	next <- currentPos
	setPos (ln-1, co)
	lbl <- name
	setPos next
	return (lbl, x)

labelOnTop' :: DgP a -> DgP (String, a)
labelOnTop' p = bimap unpack id <$> labelOnTop p

--XXX beware setting direction
-- nameAbove :: DgP Text
-- nameAbove = do
-- 	pos@(ln, co) <- currentPos
-- 	setPos (ln-1, co)
-- 	lbl <- name
-- 	setPos pos
-- 	return lbl

branch
	:: ((Tok Text) -> Bool)
	-> [(Next, DgP a)]
	->  DgP [a]
branch isFork branches = do
--  	dir0 <- psNext <$> get
	origin <- currentPos
	True <- isFork <$> peek_
-- 	traceShowM (here, "NODE!", origin) --, fmap (fmap (const ())) stuff)
	stuff <- for branches $ \(dir, p) -> do
		setDir dir
-- 		setPos origin

		--XXX fail if there is nothing under/after node!
-- 		step <|> undefined --with dir

-- 		fmap Just p <|> return Nothing
-- 		p
		(setPos origin *> step *> (Just <$> p))
		<|> return Nothing --step fail if there's nothing in desired direction
	setPos origin --eat `fork`
--  	setDir dir0 --restore direction, good for parsing boxes
	eat' --FIXME set direction!!!!!!!!!!!!!

-- 	traceShowM (here, origin, fmap (fmap (const ())) stuff)
	return $ catMaybes stuff
--	return stuff

-- branch'
-- 	:: ((Tok Text) -> Maybe b)
-- 	-> [(Next, DgP a)]
-- 	->  DgP (b, [a])
-- branch' isFork branches = do
-- 	origin <- currentPos
-- 	dir0 <- psNext <$> get
-- 	Just f <- isFork <$> peek_
-- 	stuff <- for branches $ \(dir, p) -> do
-- 		setDir dir
-- 		setPos origin
-- 		step --with dir
-- 		p
-- 	setPos origin --eat `fork`
-- 	setDir dir0 --restore direction, good for parsing boxes
-- 	eat'
-- 	return (f, stuff)

-- |Matches diagram with nothing remaining on current line
pattern DgLineEnd <- Zp _l ((_ln, Zp _ []) : _)

-- |Succeeds only when positioned on end of line
eol :: DgP ()
eol = do
-- 	p <- currentPosM
-- 	traceShowM (here, p)
	psStr <$> get >>= \case
-- 		 Zp l ((_ln, Zp _ []) : _) -> return ()
		 DgLineEnd -> return ()
		 _ -> fail here

colRight :: DgExt -> DgExt
colRight (ln, (_, co)) = (ln, (co + 1, co + 1))

colUnder :: DgExt -> DgExt
colUnder (ln, (_, co)) = (ln + 1, (co, co))

--------------------------------------------------------------------------------

hline = do
	HLine <- eat'
	return ()

vline = do
	VLine <- eat'
	return ()
	
--------------------------------------------------------------------------------

--FIXME parse it with DgP's pos type and then fmap it to 'Pos'
currentPos2 :: DgP Pos
currentPos2 = fmap Pos $ fmap (fmap fst) currentPos

extToPos :: DgExt -> Pos
extToPos = (\(ln, co) -> Pos (co, ln)) . fmap fst
	
toPos2 :: Cofree (Symbol_ String) DgExt -> Cofree (Symbol_ String) Pos
toPos2 = fmap extToPos

-- test002 :: DgP (Cofree (Symbol_ String) Pos)
-- test002 = fmap extToPos <$> test002'

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

test002' :: DgP (Cofree (Symbol_ String) DgExt)
test002'
	= setDir goDown
	*> ((:<) <$> currentPos <*> fmap Source vline'2)
	<* dgIsEmpty

node2 :: DgP (Cofree (Symbol_ String) DgExt)
node2 = (:<) <$> currentPos <*> (LadderParser.Node <$> node2')

node2' :: DgP [Cofree (Symbol_ String) DgExt]
node2'
	= branch
		(==Tokenizer.Node)
		[ (goRight, hline'2) --currentPosM>>=traceShowM>>
		, (goDown , vline'2)
		]

--FIXME with 'node2' may end only left rail, vline stemming from node must lead to another node
vline'2 :: DgP (Cofree (Symbol_ String) DgExt)
vline'2 = many vline2 *> (end2 <|> node2)

end2 :: DgP (Cofree (Symbol_ String) DgExt)
end2 = end *> ((:< End) <$> colUnder <$> lastPos)

eol2 :: DgP (Cofree (Symbol_ String) DgExt)
eol2 = eol *> ((:< Sink) <$> colRight <$> lastPos)

hline'2 :: DgP (Cofree (Symbol_ String) DgExt)
hline'2 = some hline2 *> (coil2 <|> contact2 <|> node2 <|> eol2) --TODO vline crossing

vline2 :: DgP ()
vline2 = do
	VLine <- eat'
	return ()

hline2 :: DgP () --TODO vline crossing
hline2 = do
	HLine <- eat'
	return ()

device :: DgP String -> DgP (Cofree (Symbol_ String) DgExt)
device p = do
	pos <- currentPos
	(lbl, f) <- labelOnTop' p
	(pos :<) <$> (Device f [lbl] <$> hline'2)

coil2 :: DgP (Cofree (Symbol_ String) DgExt)
coil2 = device $ do
	Coil f <- eat'
	return $ "(" ++ unpack f ++ ")"

contact2 :: DgP (Cofree (Symbol_ String) DgExt)
contact2 = device $ do
	Contact f <- eat'
	return $ "[" ++ unpack f ++ "]"

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
	Tokenizer.Node <- eat'
	return ()

edge
	= eat' >>= \t -> case t of
		REdge -> return t
		FEdge -> return t
		_ -> fail here

name = do
	Name lbl <- eat'
	return lbl

-- -- |parse left side of a box
-- leftSide = do
-- 	--VLine, REdge, FEdge, Name, connections (HLine+Name)
-- 	some leftSideBrick
-- 	return ()
-- 
-- leftSideBrick = do
-- 	(ln, co) <- currentPos
-- -- 	vline <|> edge
-- 	branch
-- 		(\case
-- 			VLine -> True
-- 			REdge -> True
-- 			FEdge -> True
-- 			_ -> False)
-- 		[ (goLeft, hline *> name)
-- 		, (goRight, name)
-- 		]
-- 	setDir goDown --i guess branch should restore direction
	
-- 	setPos (ln, co+1)
-- 	setDir goRight

box001 :: Int -> DgP ()
box001 ln = do
	setPos (ln, (1, 1))
	box

-- portName :: Int -> DgP a -> DgP (Text, a)
-- portName d p = do
-- 	(ln, co) <- currentPos
-- 	x <- p
-- 	next <- currentPos
-- 	setPos (ln, co+d)
-- 	lbl <- name
-- 	setPos next
-- 	return (lbl, x)

lwall = vline <|> void edge

-- negIn = do
-- 	NegIn <- eat'
-- 	return ()
	
--TODO check clearance
box = do
-- 	(ln, (_, co)) <- currentPos

-- 	traceShowM (here, ln, co, "------------->")
-- 	Zp zpl zpr <- psStr <$> get
-- 	forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

	setDir goUp
-- 	VLine <- eat'
-- 	currentPosM >>= (traceShowM . (here, "left wall", ))
	some lwall -- <|> negIn
-- 	currentPosM >>= (traceShowM . (here, "left top corner",))
	setDir goRight
	node
-- 	currentPosM >>= (traceShowM . (here,"top wall",))
	hline

	setDir goDown --parsing right side, look for output line position
-- 	currentPosM >>= (traceShowM . (here,"right top corner",))
	node

-- 	currentPosM >>= (traceShowM . (here,"right wall",))
	--TODO parse box instance name
	some $ do
-- 		(ln, co) <- currentPos
		vline
-- 		setPos (ln, co+1)
-- 		??? peek & record position

-- 	currentPosM >>= (traceShowM . (here,"bottom right corner",))
	setDir goLeft
	node

-- 	currentPosM >>= (traceShowM . (here,"bottom wall",))

-- 	Zp zpl zpr <- psStr <$> get
-- 	forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)


	hline
	
	
-- 	currentPosM >>= (traceShowM . (here,))
-- 	Zp zpl zpr <- psStr <$> get
-- 	forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

-- 	currentPosM >>= (traceShowM . (here,"bottom left corner",))
	setDir goUp
	node

-- 	currentPosM >>= (traceShowM . (here,"remaining left wall",))
	many lwall --0 or more

	return ()

--------------------------------------------------------------------------------

end :: DgP ()
end = do
	Nothing <- (peek . psStr) <$> get
	return ()

eat' :: DgP (Tok Text)
eat' = do
	DgPSt nx dg ps <- get
	case eat''' dg of
		 Just (v, pos, dg') -> do
			dg'' <- case nx pos dg' of
				Right q -> return q
				Left _err -> return dg' --nowhere to move
			put (DgPSt nx dg'' (Just pos))
			return v
		 Nothing -> fail $ show (here, ps)

currentPosM :: DgP (Maybe DgExt)
currentPosM = (pos . psStr) <$> get

currentPos :: DgP DgExt --(Int, (Int, Int))
currentPos = do
	Just p <- (pos . psStr) <$> get
	return p
-- 	maybe (fail "empty") (return . (,zp)) (pos zp)

peek_ :: DgP (Tok Text)
peek_ = do
	Just p <- (peek . psStr) <$> get
	return p

--------------------------------------------------------------------------------

eat''' :: Dg a -> Maybe (a, DgExt, Dg a)
eat''' (Zp u ((ln, Zp l ((col, x) : rs)) : ds))
	= Just (x, (ln, col), Zp u ((ln, Zp l rs) : ds))
eat''' _ = Nothing

pattern DgH x <- Zp _ ((_, Zp _ ((_, x) : _)) : _)
-- pattern DgH' ln cl x <- Zp _ ((ln, Zp _ ((_, x) : _)) : _)
--pattern DgM = (Zp u ((ln, Zp l ((_, x) : rs)) : ds))

peek :: Dg a -> Maybe a
peek (DgH x) = Just x
peek _ = Nothing

-- |Match on current token position
pattern DgPos ln cl cr <- Zp _ ((ln, Zp _ (((cl, cr), x) : _)) : _)

pos :: Dg a -> Maybe (Int, (Int, Int))
pos (DgPos ln cl cr) = Just (ln, (cl, cr))
pos _ = Nothing

mkDgZp :: [(Int, [((Int, Int), (Tok Text))])] -> Dg (Tok Text)
mkDgZp = Zp [] . fmap (fmap (Zp []))

--------------------------------------------------------------------------------

move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col = moveToLine line >=> moveToCol col

pattern DgLine us ln zp ds = Zp us ((ln, zp) : ds)

--FIXME merge with moveToLine somehow?
moveToCol :: Int -> Dg a -> Maybe (Dg a)
-- moveToCol col (Zp us ((ln, zp@(Zp l (((cl, cr), _) : _))) : ds))
-- moveToCol col (DgLine us ln zp@(Zp l (((cl, cr), _) : _)) ds)
-- 	| col >= cl = reassemble <$> moveTo stepRight (isIn . fst) zp
-- 	| otherwise = reassemble <$> moveTo stepLeft (isIn . fst) zp
-- 	where
-- 	isIn (a, b) = b >= col && a <= col
-- -- 	reassemble zp' = Zp us ((ln, zp') : ds)
-- 	reassemble zp' = DgLine us ln zp' ds
-- moveToCol _ _ = Nothing
moveToCol col (DgLine us ln zp ds) = reassemble <$> move2 (dir col . fst) zp
	where
	dir x (a, b)
		| x < a = LT
		| x > b = GT
		| otherwise = EQ
	reassemble zp' = DgLine us ln zp' ds
moveToCol _ _ = Nothing

moveToLine :: Int -> Dg a -> Maybe (Dg a)
moveToLine l zp = moveToLine' l (fmap (fmap foc) (foc zp))

moveToLine' :: Int -> Dg a -> Maybe (Dg a)
moveToLine' ln = move2 (compare ln . fst)
-- moveToLine' line zp@(Zp _ ( (ln, _) : _))
-- 	| line >= ln = moveTo stepRight ((line==).fst) zp
-- 	| otherwise = moveTo stepLeft ((line==).fst) zp
-- moveToLine' _ _ = Nothing

--------------------------------------------------------------------------------
