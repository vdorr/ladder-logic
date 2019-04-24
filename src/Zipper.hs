{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances,
	PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable,
	LambdaCase, ScopedTypeVariables, ViewPatterns #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Zipper where

import Prelude hiding (fail)
-- import System.Environment (getArgs)
-- import qualified Data.Text.IO as TIO
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)
import Data.Traversable
-- import Data.Foldable
import Data.Text (Text, unpack)
import Data.Bifunctor
import Data.Maybe

import Preprocess
import LadderParser hiding (node, hline, Node, device)
import qualified LadderParser
import DiagramParser (Pos(..))

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

#if 0
move'' :: (a -> Ordering) -> Zp a -> Maybe (Zp a)
move'' f (foc -> zp@(Zp _ (x : xs)))
	= case f x of
		LT -> moveTo stepLeft ((==EQ).f) zp
		_ -> moveTo stepRight ((==EQ).f) zp
move'' _ _ = Nothing
#endif

-- |Drop empty lines
dgTrim :: Dg a -> Dg a
dgTrim (Zp l r) = Zp (filter (not.zpNull.snd) l) (filter (not.zpNull.snd) r)

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
	, psStr :: Dg Tok
	, psLastBite :: Maybe DgExt -- ^position of last token eaten
	}

lastPos :: DgP DgExt
lastPos = psLastBite <$> get >>= maybe (fail here) return


applyDgp :: DgP a -> Dg Tok -> Either String (a, DgPSt)
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

--------------------------------------------------------------------------------

setDir :: Next -> DgP ()
setDir f = modify $ \(DgPSt _ zp ps) -> DgPSt f zp ps

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
type Next = (Int, (Int, Int)) -> Dg Tok -> Either String (Dg Tok)

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
	:: (Tok -> Bool)
	-> [(Next, DgP a)]
	->  DgP [a]
branch isFork branches = do
--  	dir0 <- psNext <$> get
	origin <- currentPos
	True <- isFork <$> peek_
	stuff <- for branches $ \(dir, p) -> do
		setDir dir
		setPos origin

		--XXX fail if there is nothing under/after node!
-- 		step <|> undefined --with dir

-- 		fmap Just p <|> return Nothing
-- 		p
		fmap Just (step *> p) <|> return Nothing --step fail if there's nothing in desired direction
	setPos origin --eat `fork`
--  	setDir dir0 --restore direction, good for parsing boxes
	eat' --FIXME set direction!!!!!!!!!!!!!

 	return $ catMaybes stuff
--	return stuff

-- branch'
-- 	:: (Tok -> Maybe b)
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
pattern DgEmpty <- Zp _l ((_ln, Zp _ []) : _)

-- |Succeeds only when positioned on end of line
eol :: DgP ()
eol = do
	p <- currentPosM
	traceShowM (here, p)
	psStr <$> get >>= \case
-- 		 Zp l ((_ln, Zp _ []) : _) -> return ()
		 DgEmpty -> return ()
		 _ -> fail here

--------------------------------------------------------------------------------

test001 :: DgP ()
test001 = do
	setDir goDown
	some vline
	node' <|> end
	dgIsEmpty

node' :: DgP ()
node'
	= () <$ branch
		(\case
			Node -> True
			_ -> False)
		[ (goRight, hline')
		, (goDown, vline')
		]

vline' :: DgP ()
-- vline' = some (node' <|> vline)
vline' = many vline *> (node' <|> end)

hline' = do
	many (coil <|> hline <|> contact <|> node') --TODO vline crossing
	return ()


coil = do
	labelOnTop $ do
		Coil _ <- eat'
		return ()
	return ()

contact = do
-- 	Contact _ <- eat'
	labelOnTop $ do
		Contact _ <- eat'
		return ()
	return ()

--------------------------------------------------------------------------------

hline = do
	HLine <- eat'
	return ()

vline = do
	VLine <- eat'
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

currentPos2 :: DgP Pos
currentPos2 = fmap Pos $ fmap (fmap fst) currentPos

test002 :: DgP (Cofree Symbol_ Pos)
test002 = do
-- 	setDir goDown *> vline2 <* dgIsEmpty
	q <- setDir goDown *> vline2
	Zp zpl zpr <- psStr <$> get
-- 	traceShowM (here, zp)
	forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

	dgIsEmpty
	return q

node2 :: DgP (Cofree Symbol_ Pos)
node2 = (:<) <$> currentPos2 <*> (LadderParser.Node <$> node2')

node2' :: DgP [Cofree Symbol_ Pos]
node2'
	= branch
		(==Preprocess.Node)
		[ (goRight, currentPosM>>=traceShowM>>hline'2)
		, (goDown, currentPosM>>=traceShowM>>vline'2)
		]

vline'2 :: DgP (Cofree Symbol_ Pos)
vline'2 = many vline2 *> (end2 <|> node2)

end2 :: DgP (Cofree Symbol_ Pos)
end2 = Pos (-1,-1) :< Sink <$ end

hline'2 :: DgP (Cofree Symbol_ Pos)
hline'2 = do
	some hline2
	p <- currentPosM
	traceShowM (here, p, ">>>>>>>")
	coil2 <|> contact2 <|> node2 <|> eol2 --TODO vline crossing

eol2 :: DgP (Cofree Symbol_ Pos)
eol2 = do
	p <- currentPosM
	traceShowM (here, p)
	(:< Sink) <$> (fmap (\(ln, (_, co)) -> Pos (ln, co)) lastPos) <* eol

vline2 :: DgP (Cofree Symbol_ Pos)
vline2 = do
	VLine <- eat'
	vline'2

hline2 :: DgP ()
hline2 = do
	HLine <- eat'
	return ()

device :: DgP String -> DgP (Cofree Symbol_ Pos)
device p = do
	pos <- currentPos2
	(lbl, f) <- labelOnTop' p
	(pos :<) <$> (Device f [lbl] <$> hline'2)

coil2 :: DgP (Cofree Symbol_ Pos)
coil2 = device $ do
	Coil f <- eat'
	return $ "(" ++ unpack f ++ ")"

contact2 :: DgP (Cofree Symbol_ Pos)
contact2 = do
	p <- currentPosM
	traceShowM (here, p)
	device $ do
		Contact f <- eat'
		p <- currentPosM
		traceShowM (here, p)
		return $ "[" ++ unpack f ++ "]"

-- coil2 :: DgP (Cofree Symbol_ Pos)
-- coil2 = do
-- 	p <- currentPos2
-- 	(lbl, f) <- labelOnTop $ do
-- 		Coil f <- eat'
-- 		return f
-- 	(p :<) <$> (Device ("(" ++ unpack f ++ ")") [unpack lbl] <$> hline'2)

-- contact2 :: DgP (Cofree Symbol_ Pos)
-- contact2 = do
-- 	p <- currentPos2
-- 	(lbl, f) <- labelOnTop $ do
-- 		Contact f <- eat'
-- 		return f
-- 	(p :<) <$> (Device ("[" ++ unpack f ++ "]") [unpack lbl] <$> hline'2)
	
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
	Preprocess.Node <- eat'
	return ()

edge
	= eat' >>= \t -> case t of
		REdge -> return t
		FEdge -> return t
		_ -> fail here

name = do
	Name lbl <- eat'
	return lbl

-- |parse left side of a box
leftSide = do
	--VLine, REdge, FEdge, Name, connections (HLine+Name)
	some leftSideBrick
	return ()

leftSideBrick = do
	(ln, co) <- currentPos
-- 	vline <|> edge
	branch
		(\case
			VLine -> True
			REdge -> True
			FEdge -> True
			_ -> False)
		[ (goLeft, hline *> name)
		, (goRight, name)
		]
	setDir goDown --i guess branch should restore direction
	
-- 	setPos (ln, co+1)
-- 	setDir goRight

box001 :: Int -> DgP ()
box001 ln = do
	setPos (ln, (1, 1))
	box

--TODO check clearance
box = do
	(ln, (_, co)) <- currentPos

	traceShowM (here, ln, co, "------------->")
	Zp zpl zpr <- psStr <$> get
	forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

	setDir goUp
-- 	VLine <- eat'
	currentPosM >>= (traceShowM . (here, "left wall", ))
	some vline
	currentPosM >>= (traceShowM . (here, "left top corner",))
	setDir goRight
	node
	currentPosM >>= (traceShowM . (here,"top wall",))
	hline

	setDir goDown --parsing right side, look for output line position
	currentPosM >>= (traceShowM . (here,"right top corner",))
	node

	currentPosM >>= (traceShowM . (here,"right wall",))
	--TODO parse box instance name
	some $ do
-- 		(ln, co) <- currentPos
		vline
-- 		setPos (ln, co+1)
-- 		??? peek & record position

	currentPosM >>= (traceShowM . (here,"bottom right corner",))
	setDir goLeft
	node

	currentPosM >>= (traceShowM . (here,"bottom wall",))

	Zp zpl zpr <- psStr <$> get
	forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)


	hline
	
	
	currentPosM >>= (traceShowM . (here,))
	Zp zpl zpr <- psStr <$> get
	forM_ (reverse zpl ++ zpr) $ \q -> traceShowM (here, q)

	currentPosM >>= (traceShowM . (here,"bottom left corner",))
	setDir goUp
	node

	currentPosM >>= (traceShowM . (here,"remaining left wall",))
	many vline --0 or more

	return ()

--------------------------------------------------------------------------------

end :: DgP ()
end = do
	p <- currentPosM
	x <- (peek . psStr) <$> get
	traceShowM (here, p, x)
	Nothing <- (peek . psStr) <$> get
	return ()

eat' :: DgP Tok
eat' = do
	DgPSt nx dg ps <- get
	case eat''' dg of
		 Just (v, pos, dg') -> do
			dg'' <- case nx pos dg' of
				Right q -> return q
				Left _err -> return dg' --nowhere to move
			put (DgPSt nx dg'' (Just pos))
			return v
		 Nothing -> fail here

-- eat (Zp us ((ln, Zp l ((_, x) : rs)) : ds)) = Just (x, Zp us ((ln, Zp l rs) : ds))
-- eat _ = Nothing

eat''' :: Dg a -> Maybe (a, DgExt, Dg a)
eat''' (Zp u ((ln, Zp l ((col, x) : rs)) : ds))
	= Just (x, (ln, col), Zp u ((ln, Zp l rs) : ds))
eat''' _ = Nothing

currentPosM :: DgP (Maybe DgExt)
currentPosM = (pos . psStr) <$> get

currentPos :: DgP DgExt --(Int, (Int, Int))
currentPos = do
	Just p <- (pos . psStr) <$> get
	return p
-- 	maybe (fail "empty") (return . (,zp)) (pos zp)

peek_ :: DgP Tok
peek_ = do
	Just p <- (peek . psStr) <$> get
	return p

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

mkDgZp :: [(Int, [((Int, Int), Tok)])] -> Dg Tok
mkDgZp = Zp [] . fmap (fmap (Zp []))

--------------------------------------------------------------------------------

move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col = moveToLine line >=> moveToCol col

moveToCol :: Int -> Dg a -> Maybe (Dg a)
moveToCol col (Zp us ((ln, zp@(Zp l (((cl, cr), _) : _))) : ds))
	| col >= cl = reassemble <$> moveTo stepRight (isIn . fst) zp
	| otherwise = reassemble <$> moveTo stepLeft (isIn . fst) zp
	where
	isIn (a, b) = b >= col && a <= col
	reassemble zp' = Zp us ((ln, zp') : ds)
moveToCol _ _ = Nothing

moveToLine :: Int -> Dg a -> Maybe (Dg a)
moveToLine l zp = moveToLine' l (fmap (fmap foc) (foc zp))

moveToLine' :: Int -> Dg a -> Maybe (Dg a)
moveToLine' line zp@(Zp _ ( (ln, _) : _))
	| line >= ln = moveTo stepRight ((line==).fst) zp
	| otherwise = moveTo stepLeft ((line==).fst) zp
moveToLine' _ _ = Nothing

--------------------------------------------------------------------------------
