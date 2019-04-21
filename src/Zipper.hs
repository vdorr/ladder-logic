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

import Preprocess
import LadderParser hiding (node, hline, Node)
import qualified LadderParser
import DiagramParser (Pos(..))

import Debug.Trace

--------------------------------------------------------------------------------

data Zp a = Zp [a] [a]
	deriving (Show, Functor, Eq) -- , Foldable)

zpFromList :: [a] -> Zp a
zpFromList = Zp []

zpToList :: Zp a -> [a]
zpToList (Zp l r) = reverse l ++ r

zpLength :: Zp a -> Int
zpLength (Zp l r) = length l + length r

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

--------------------------------------------------------------------------------

-- |Diagram parser input stream
type Dg a = Zp (Int, Zp ((Int, Int), a))

-- |Returns number of remaining tokens
dgLength :: Dg a -> Int
dgLength (Zp l r) = sum (fmap (zpLength.snd) l) + sum (fmap (zpLength.snd) r)

type DgPSt = (Next, Dg Tok)

applyDgp p dg = dgp p (goRight, dg)

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

get = DgP $ \s -> return (s, s)
put s = DgP $ \_ -> return ((), s)
modify f = f <$> get >>= put

--------------------------------------------------------------------------------

setDir :: Next -> DgP ()
setDir f = modify $ \(_, zp) -> (f, zp)

step :: DgP ()
step = do
	origin <- currentPos
	(f, zp) <- get
	case f origin zp of
		Right zp' -> put (f, zp')
		Left err -> fail here --or not?

setPos :: (Int, (Int, b)) -> DgP ()
setPos (ln, (co, _)) = do
	(b, zp) <- get
	Just zp' <- return $ move ln co zp --FIXME can only move to direct neighbour!!!!!!!
	put (b, zp')

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
goLeft (ln, (_, co)) = move_ ln (co-1)

--------------------------------------------------------------------------------

-- |Fail if input stream is not empty
dgIsEmpty :: DgP ()
dgIsEmpty
	= (dgLength . snd) <$> get
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

branch
	:: (Tok -> Bool)
	-> [(Next, DgP a)]
	->  DgP [a]
branch isFork branches = do
--  	dir0 <- fst <$> get
	origin <- currentPos
	True <- isFork <$> peek_
	stuff <- for branches $ \(dir, p) -> do
		setDir dir
		setPos origin
		step --with dir
		p
	setPos origin --eat `fork`
--  	setDir dir0 --restore direction, good for parsing boxes
	eat' --FIXME set direction!!!!!!!!!!!!!
	return stuff

-- branch'
-- 	:: (Tok -> Maybe b)
-- 	-> [(Next, DgP a)]
-- 	->  DgP (b, [a])
-- branch' isFork branches = do
-- 	origin <- currentPos
-- 	dir0 <- fst <$> get
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

--------------------------------------------------------------------------------

test001 :: DgP () --(Cofree Symbol_ Pos)
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

hline = do
	HLine <- eat'
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

vline = do
	VLine <- eat'
	return ()

node = do
	Preprocess.Node <- eat'
	return ()
	
--------------------------------------------------------------------------------

test002 :: DgP (Cofree Symbol_ Pos)
test002 = do
	setDir goDown
	traceShowM (here)
	vline2 -- <* dgIsEmpty

node'2' :: DgP (Cofree Symbol_ Pos)
node'2' = traceShowM (here) >> xxx (LadderParser.Node <$> node'2)

node'2 :: DgP [Cofree Symbol_ Pos]
node'2
	= branch
		(==Preprocess.Node)
		[ (goRight, traceShowM (here) >> hline'2)
		, (goDown, traceShowM (here) >> vline'2)
		]

currentPos2 = fmap Pos $ fmap (fmap fst) currentPos
xxx p = (:<) <$> currentPos2 <*> p
-- xxx (some vline2)

vline'2 :: DgP (Cofree Symbol_ Pos)
vline'2
	= traceShowM (here) >> many vline2 *> (node'2' <|> end2)

end2 :: DgP (Cofree Symbol_ Pos)
end2 = xxx (Sink <$ end)

hline'2 :: DgP (Cofree Symbol_ Pos)
hline'2 = do
	p <- currentPos
	traceShowM (here, p)
	coil2 <|> hline2 <|> contact2 <|> node'2' <|> eol2 --TODO vline crossing

eol2 :: DgP (Cofree Symbol_ Pos)
eol2 = do
	p <- currentPos
	zp <- snd <$> get
	traceShowM (here, p, zp)
	case zp of
		 Zp l ((_ln, Zp _ []) : _) -> undefined
		 _ -> fail here
-- Zp (Int, Zp ((Int, Int), a))

hline2 :: DgP (Cofree Symbol_ Pos)
hline2 = do
	traceShowM (here)
	HLine <- eat'
	p <- currentPos
	traceShowM (here, p)
	hline'2 <|> eol2

coil2 :: DgP (Cofree Symbol_ Pos)
coil2 = do
	traceShowM (here)
	pp <- currentPos
	p <- currentPos2
	(lbl, f) <- labelOnTop $ do
		Coil f <- eat'
		traceShowM (here, pp)
		return f
	traceShowM (here)
	(p :<) <$> (Device ("("++unpack f++")") [unpack lbl]
		<$> (traceShowM (here) >>(hline'2 
			<* traceShowM (here))))

contact2 :: DgP (Cofree Symbol_ Pos)
contact2 = do
	traceShowM (here)
-- 	Contact _ <- eat'
	p <- currentPos2
	(lbl, f) <- labelOnTop $ do
		Contact f <- eat'
		traceShowM (here)
		return f
	(p :<) <$> (Device ("["++unpack f++"]") [unpack lbl] <$> hline'2)

vline2 :: DgP (Cofree Symbol_ Pos)
vline2 = do
	VLine <- eat'
	traceShowM (here)
	vline'2

-- node2 :: DgP ()
-- node2 = do
-- 	Preprocess.Node <- eat'
-- 	return ()
	
--------------------------------------------------------------------------------

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




--TODO check clearance
box = do
	(ln, (_, co)) <- currentPos

	setDir goUp
-- 	VLine <- eat'
	some vline
	node

	setDir goRight
	hline
	node
	--TODO parse box instance name

	setDir goDown --parsing right side, look for output line position
	some $ do
-- 		(ln, co) <- currentPos
		vline
-- 		setPos (ln, co+1)
-- 		??? peek & record position
	node

	setDir goLeft
	hline
	node

	setDir goUp
	many vline --0 or more

	return ()

--------------------------------------------------------------------------------

end :: DgP ()
end = do
	s <- snd <$> get
-- 	s <- (peek . snd) <$> get
	traceShowM (here, peek s)
	case peek s of
		Nothing -> do
-- 			traceShowM (here, s)
			return ()
		Just _ -> do
			traceShowM (here, s)
			return ()

	Nothing <- (peek . snd) <$> get
	return ()

eat' :: DgP Tok
eat' = do
	(nx, dg) <- get
	case eat''' dg of
		 Just (v, (ln, co), dg') -> do
			dg'' <- case nx (ln, co) dg' of
				Right q -> return q
				Left _err -> return dg' --nowhere to move
			put (nx, dg'')
			return v
		 Nothing -> fail here

currentPos :: DgP (Int, (Int, Int))
currentPos = do
	Just p <- (pos . snd) <$> get
	return p
-- 	maybe (fail "empty") (return . (,zp)) (pos zp)

peek_ = do
	Just p <- (peek . snd) <$> get
	return p

pattern DgH x <- Zp _ ((_, Zp _ ((_, x) : _)) : _)
-- pattern DgH' ln cl x <- Zp _ ((ln, Zp _ ((_, x) : _)) : _)
pattern DgPos ln cl cr <- Zp _ ((ln, Zp _ (((cl, cr), x) : _)) : _)
--pattern DgM = (Zp u ((ln, Zp l ((_, x) : rs)) : ds))

peek :: Dg a -> Maybe a
peek (DgH x) = Just x
peek _ = Nothing

-- eat (Zp us ((ln, Zp l ((_, x) : rs)) : ds)) = Just (x, Zp us ((ln, Zp l rs) : ds))
-- eat _ = Nothing

eat''' (Zp u ((ln, Zp l ((col, x) : rs)) : ds))
	= Just (x, (ln, col), Zp u ((ln, Zp l rs) : ds))
eat''' _ = Nothing

pos :: Dg a -> Maybe (Int, (Int, Int))
pos (DgPos ln cl cr) = Just (ln, (cl, cr))
pos _ = Nothing

mkDgZp :: [(Int, [((Int, Int), Tok)])] -> Dg Tok
mkDgZp = Zp [] . fmap (fmap (Zp []))

--------------------------------------------------------------------------------

move'' :: (a -> Ordering) -> Zp a -> Maybe (Zp a)
move'' f (foc -> zp@(Zp _ (x : xs)))
	= case f x of
		LT -> moveTo stepLeft ((==EQ).f) zp
		_ -> moveTo stepRight ((==EQ).f) zp

move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col
	= moveToLine line --(fmap (fmap foc) (foc zp))
	>=> moveToCol col

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
