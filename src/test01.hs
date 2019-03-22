{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances,
	PatternSynonyms, DeriveFunctor, DeriveFoldable, DeriveTraversable,
	LambdaCase, ScopedTypeVariables #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Prelude hiding (fail)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)
import Data.Traversable
import Data.Foldable

import Preprocess
import LadderParser hiding (node, hline, Node)
import DiagramParser (Pos)

import Debug.Trace

data Zp a = Zp [a] [a]
	deriving (Show, Functor) -- , Foldable)

zpFromList :: [a] -> Zp a
zpFromList = Zp []

type Dg a = Zp (Int, Zp ((Int, Int), a))

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
-- 	traceShowM here
	setDir goDown
-- 	traceShowM here
	VLine <- eat'

	branch
		(\case
			Node -> True
			_ -> False)
		[ (goRight, hline' >> return () )
		, (goDown, return () -- <|> pure ()
			)
		]
-- 		<|> pure []

	return ()

branch
	:: (Tok -> Bool)
	-> [(Next, DgP a)]
	->  DgP [a]
branch isFork branches = do
	x <- currentPos
-- 	traceShowM (here, x)
	True <- isFork <$> peek'
-- 	traceShowM (here)
	stuff <- for branches $ \(dir, p) -> do
-- 		traceShowM (here)
		setDir dir
-- 		traceShowM (here)
		setPos x
		traceShowM (here)
		step --with dir
		traceShowM (here)
		p
	setPos x --eat `fork`
	eat' --FIXME set direction!!!!!!!!!!!!!
	return stuff

setDir f = do
	(a, _, zp) <- get
	put (a, f, zp)

step = do
	x <- currentPos
	(a, f, zp) <- get
-- 	traceShowM (here, x)
	case f x zp of
		Right zp' -> put (a, f, zp')
		Left err -> do
			traceShowM (here, err)
			fail here

setPos (q, (w, _FIXME)) = do
	(a, b, zp) <- get
	Just zp' <- return $ move q w zp --FIXME can only move to direct neighbour!!!!!!!
	put (a, b, zp')

-- |push current position, apply parser and restore it to the next from stored
-- fromHere p = do
-- 	(a, b, c) <- get
-- 	pp <- pos'
-- 	put (pp:a, b, c) --can i just keep position in local var?
-- 	x <- p
-- 	put (a, b, c)
-- 	return x

--move in some direction from provided origin
type Next = (Int, (Int, Int)) -> Dg Tok -> Either String (Dg Tok)

goRight :: Next
goRight (ln, (_, co)) dg
	= case move ln (co+1) dg of
		Just zp' -> return zp'
		Nothing -> do
-- 			traceShowM (here, dg)
-- 			traceShowM (here, ln, (co+1))
			Left here

goDown :: Next
goDown (ln, (co, _)) dg
	= case move (ln+1) co dg of
		Just zp' -> return zp'
		Nothing -> do
-- 			traceShowM (here, dg)
-- 			traceShowM (here, (ln+1), co)
			Left here

get = DgP $ \s -> return (s, s)
put s = DgP $ \_ -> return ((), s)

hline' = do
	many
		$ coil
		<|> hline
		<|> contact
	p <- currentPos
	q <- peek'
-- 	traceShowM (here, p, q)
	return []
hline = do
	p <- currentPos
	q <- peek'
	traceShowM (here, p, q)
	
	HLine <- eat'
	traceShowM (here)
	return []
coil = do
	Coil _ <- eat'
	return []
contact = do
	Contact _ <- eat'
	return []

vline = do
	VLine <- eat'
	return []

node = do
	Preprocess.Node <- eat'
	return ()

eat' :: DgP Tok
eat' = do --DgP ((maybe (Left "empty") Right) . eat)
	(stk, nx, dg@(DgH x)) <- get
	case eat''' dg of
		 Just (v, (ln, co), dg') -> do
-- 			 (stk, nx, dg)
			traceShowM (here, ln, co)
-- 			traceShowM (here, dg')
			dg'' <- case nx (ln, co) dg' of
				Right q -> return q
				Left err -> do
-- 					traceShowM (here, err)
-- 					fail err
					traceShowM (here, "nowhere to move")
					return dg'
			put (stk, nx, dg'')
			return v
		 Nothing -> undefined

currentPos :: DgP (Int, (Int, Int))
currentPos = pos'

pos' = do
	(_, _, zp) <- get
	Just p <- return $ pos zp
	return p
-- 	maybe (fail "empty") (return . (,zp)) (pos zp)
peek' = do
	(_, _, zp) <- get
	Just p <- return $ peek zp
	return p

pattern DgH x <- Zp _ ((_, Zp _ ((_, x) : _)) : _)
-- pattern DgH' ln cl x <- Zp _ ((ln, Zp _ ((_, x) : _)) : _)
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

pos :: Dg a -> Maybe (Int, (Int, Int))
pos (DgPos ln cl cr) = Just (ln, (cl, cr))
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
	isIn (a, b) = (b>=col)&&(a<=col)
	reassemble zp' = Zp us ((ln, zp') : ds)
moveToCol _ _ = traceShowM here >> Nothing

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
moveTo _ _ _ = Nothing

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
			let zp@(Zp zpl zpr) = mkDgZp x

-- 			print (here, zp)
			for_ zpl $ \q -> print (here, q)
			for_ zpr $ \q -> print (here, q)
			
			case applyDgp test001 zp of
				Right (_, (a,_,c@(Zp zpl zpr))) -> do
-- 					print (here, a, c)
					for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
-- 					for_ zpr $ \q -> print (here, q)
				Left err -> print (here, err)
#if 0
			forM_ x $ \(l,c) -> do
				print l
				print c
#endif
