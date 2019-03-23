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

--------------------------------------------------------------------------------

data Zp a = Zp [a] [a]
	deriving (Show, Functor) -- , Foldable)

zpFromList :: [a] -> Zp a
zpFromList = Zp []

type Dg a = Zp (Int, Zp ((Int, Int), a))

peek :: Dg a -> Maybe a
eat :: Dg a -> Maybe (a, Dg a)

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
modify f = (f <$> get) >>= put

--------------------------------------------------------------------------------

setDir f = do
	(_, zp) <- get
	put (f, zp)

step = do
	origin <- currentPos
	(f, zp) <- get
	case f origin zp of
		Right zp' -> put (f, zp')
		Left err -> fail here --or not?

setPos (ln, (co, _)) = do
	(b, zp) <- get
	Just zp' <- return $ move ln co zp --FIXME can only move to direct neighbour!!!!!!!
	put (b, zp')

--move in some direction from provided origin
type Next = (Int, (Int, Int)) -> Dg Tok -> Either String (Dg Tok)

move_ :: Int -> Int -> Dg a -> Either String (Dg a)
move_ ln co dg = maybe (Left here) return (move ln co dg)

goRight :: Next
goRight (ln, (_, co)) = move_ ln (co+1)
-- dg
-- 	= case move ln (co+1) dg of
-- 		Just zp' -> return zp'
-- 		Nothing -> Left here

goDown :: Next
goDown (ln, (co, _)) = move_ (ln+1) co
-- dg
-- 	= case move (ln+1) co dg of
-- 		Just zp' -> return zp'
-- 		Nothing -> Left here

--------------------------------------------------------------------------------

test001 :: DgP () --(Cofree Symbol_ Pos)
test001 = do
	setDir goDown
	vline
	node'
	return ()

node'
	= branch
		(\case
			Node -> True
			_ -> False)
		[ (goRight, hline' >> return () )
		, (goDown, vline' >> return () )
		]

vline' = some (node' <|> vline)

branch
	:: (Tok -> Bool)
	-> [(Next, DgP a)]
	->  DgP [a]
branch isFork branches = do
	origin <- currentPos
	True <- isFork <$> peek'
	stuff <- for branches $ \(dir, p) -> do
		setDir dir
		setPos origin
		step --with dir
		p
	setPos origin --eat `fork`
	eat' --FIXME set direction!!!!!!!!!!!!!
	return stuff

hline' = do
	many (coil <|> hline <|> contact <|> node')
	return []
hline = do	
	HLine <- eat'
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

--------------------------------------------------------------------------------

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
peek' = do
	Just p <- (peek . snd) <$> get
	return p

pattern DgH x <- Zp _ ((_, Zp _ ((_, x) : _)) : _)
-- pattern DgH' ln cl x <- Zp _ ((ln, Zp _ ((_, x) : _)) : _)
pattern DgPos ln cl cr <- Zp _ ((ln, Zp _ (((cl, cr), x) : _)) : _)
--pattern DgM = (Zp u ((ln, Zp l ((_, x) : rs)) : ds))

--peek (Zp _ ((_, Zp _ ((_, x) : _)) : _)) = Just x
peek (DgH x) = Just x
peek _ = Nothing

eat (Zp us ((ln, Zp l ((_, x) : rs)) : ds)) = Just (x, Zp us ((ln, Zp l rs) : ds))
eat _ = Nothing

eat''' (Zp u ((ln, Zp l ((col, x) : rs)) : ds))
	= Just (x, (ln, col), Zp u ((ln, Zp l rs) : ds))
eat''' _ = Nothing

pos :: Dg a -> Maybe (Int, (Int, Int))
pos (DgPos ln cl cr) = Just (ln, (cl, cr))
pos _ = Nothing

-- move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col zp
	= moveToLine line zp --(fmap (fmap foc) (foc zp))
	>>= moveToCol col

moveToCol :: Int -> Dg a -> Maybe (Dg a)
moveToCol col (Zp us ((ln, zp@(Zp l (((cl, cr), x) : rs))) : ds))
	| col >= cl = reassemble <$> moveTo stepRight (isIn . fst) zp
	| col <= cl = reassemble <$> moveTo stepLeft (isIn . fst) zp
	where
	isIn (a, b) = (b>=col)&&(a<=col)
	reassemble zp' = Zp us ((ln, zp') : ds)
moveToCol _ _ = Nothing

-- |Bring something into focus
foc :: Zp a -> Zp a
foc (Zp (x:xs) []) = Zp xs [x]
foc zp = zp

moveToLine :: Int -> Dg a -> Maybe (Dg a)
moveToLine l zp = moveToLine' l (fmap (fmap foc) (foc zp))

-- moveToLine' :: Int -> Dg a -> Maybe (Dg a)
moveToLine' line zp@(Zp _ ( (ln, _) : _))
	| line >= ln = moveTo stepRight ((line==).fst) zp
	| otherwise = moveTo stepLeft ((line==).fst) zp
moveToLine' _ _ = Nothing

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

--------------------------------------------------------------------------------

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
				Right (_, (_,c@(Zp zpl zpr))) -> do
-- 					print (here, a, c)
					for_ (reverse zpl ++ zpr) $ \q -> print (here, q)
-- 					for_ zpr $ \q -> print (here, q)
				Left err -> print (here, err)
#if 0
			forM_ x $ \(l,c) -> do
				print l
				print c
#endif
