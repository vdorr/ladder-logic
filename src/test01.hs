{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances,
	PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Prelude hiding (fail)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)

import Preprocess
import LadderParser hiding (node, hline)
import DiagramParser (Pos)

data Zp a = Zp [a] [a]
	deriving (Show, Functor) -- , Foldable)

zpFromList :: [a] -> Zp a
zpFromList = Zp []

-- instance Functor Zp where
-- 	fmap f (Zp l r) = Zp (fmap f l) (fmap f r)

type Dg a = Zp (Int, Zp ((Int, Int), a))

up, down, left, right :: Dg a -> Maybe (Dg a)

up = undefined
down = undefined
left = undefined
right = undefined

peek :: Dg a -> Maybe a
eat :: Dg a -> Maybe (a, Dg a)

type DgPSt = ([(Int, Int)], Next, Dg Tok)

applyDgp p dg = dgp p ([], goRight, dg)

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

test001 :: DgP () --(Cofree Symbol_ Pos)
test001 = do
	down'
	VLine <- eat' --down vline
-- 	down vline <|> (node *> (++) <$> right hline <*> down vline)
	vline
		<|> fromHere (right' *> node *>
			((++) <$> hline <*> (down' *> restorePos *> vline)))
	return ()

branch
	::
	(Monoid a) => -- ???
	=>
	(Tok -> Bool) -- ??
	-> [(Next, DgP a)]
	->  DgP a
branch fork branches = do
	x <- currentPos
	peek??? fork???
	stuff <- for branches $ \(dir, p) -> do
		setDir dir
		setPos x
		step --with dir
		p
	eat `fork`
	undefined
	return $ sconcat stuff
-- [...] [a,...] -> [a,...] [...]
-- then apply parsers and eat branch point

-- |push current position, apply parser and restore it to the next from stored
fromHere p = do
	(a, b, c) <- get
	pp <- pos'
	put (pp:a, b, c) --can i just keep position in local var?
	x <- p
	put (a, b, c)
	return x

--move in some direction from provided origin
type Next = (Int, Int) -> Dg Tok -> Either String (Dg Tok)

goRight :: Next
goRight (ln, co) dg
	= case move ln (co+1) dg of
		Just _ -> undefined
		Nothing -> undefined

restorePos = undefined

get = DgP $ \s -> return (s, s)
put s = DgP $ \_ -> return ((), s)

-- |
from :: DgP a -> DgP a
from = undefined

hline = undefined
down' = return ()
right' = return ()

vline = do
-- 	p <- pos'
	VLine <- eat'
	return []

node = do
-- 	p <- pos'
	Preprocess.Node <- eat'
	return ()

-- variant of eat that check position of next token
eat'' = undefined

eat' :: DgP Tok
eat' = do --DgP ((maybe (Left "empty") Right) . eat)
	(stk, nx, dg) <- get
	case eat''' dg of
		 Just (v, (ln, (co, _)), dg') -> do
-- 			 (stk, nx, dg)
			Right dg'' <- return $ nx (ln, co) dg'
			put (stk, nx, dg'')
			return v
		 Nothing -> undefined

pos' = do
	(_, _, zp) <- get
	Just p <- return $ pos zp
	return p
-- 	maybe (fail "empty") (return . (,zp)) (pos zp)

pattern DgH x <- Zp _ ((_, Zp _ ((_, x) : _)) : _)
pattern DgH' ln x <- Zp _ ((ln, Zp _ ((_, x) : _)) : _)
pattern DgPos ln cl cr <- Zp _ ((ln, Zp _ (((cl, cr), x) : _)) : _)
--pattern DgM = (Zp u ((ln, Zp l ((_, x) : rs)) : ds))

--peek (Zp _ ((_, Zp _ ((_, x) : _)) : _)) = Just x
peek (DgH x) = Just x
peek _ = Nothing

-- eat = undefined
eat (Zp us ((ln, Zp l ((_, x) : rs)) : ds)) = Just (x, Zp us ((ln, Zp l rs) : ds))
eat _ = Nothing

eat''' (Zp u ((ln, Zp l ((col, x) : rs)) : ds))
	= Just (x, (ln, col), Zp u ((ln, Zp l rs) : ds))
eat''' _ = Nothing

pos :: Dg a -> Maybe (Int, Int)
pos (DgPos ln cl _) = Just (ln, cl)
pos _ = Nothing
-- pos dg = do
-- 	DgPos ln cl _ <- return dg
-- 	return (ln, cl)

move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col zp
	= moveToLine line zp
	>>= moveToCol col

moveToCol :: Int -> Dg a -> Maybe (Dg a)
-- moveToCol col zp@((Zp u ((ln, Zp l (((cl, cr), x) : rs)) : ds)))
moveToCol col (Zp us ((ln, zp@(Zp l (((cl, cr), x) : rs))) : ds))
	| col >= cl = reassemble <$> moveTo stepRight (isIn . fst) zp
	| col <= cl = reassemble <$> moveTo stepLeft (isIn . fst) zp
	where
	isIn (a, b) = undefined
	reassemble zp' = Zp us ((ln, zp') : ds)
moveToCol _ _ = Nothing

moveToLine :: Int -> Dg a -> Maybe (Dg a)
moveToLine line zp@(DgPos ln _ _)
	| line >= ln = moveTo stepRight ((line==).fst) zp
	| otherwise = moveTo stepLeft ((line==).fst) zp
moveToLine _ _ = Nothing

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
moveTo :: (Zp a -> Maybe (Zp a)) -> (a -> Bool) -> Zp a -> Maybe (Zp a)
moveTo move test zp@(ZpR l foc r) -- = undefined
	| test foc = pure zp
	| otherwise = move zp >>= moveTo move test

mkDgZp
	:: [(Int, [((Int, Int), Tok)])]
	-> Dg Tok
mkDgZp = Zp [] . fmap (fmap (Zp []))

main = do
	[file] <- getArgs
	src <- TIO.readFile file
	case stripPos <$> preproc4 src of
		Left err -> TIO.putStrLn err
		Right x -> do
-- 			print $ stripPos x
			let Right (_, (a,_,c)) = applyDgp test001 $ mkDgZp x
				in print (here, a, c)
			forM_ x $ \(l,c) -> do
				print l
				print c
