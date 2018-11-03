{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-incomplete-patterns
                     -fwarn-unused-binds
                     -fwarn-unused-imports #-}

module DiagramParser where

import Control.Monad.State
import Control.Monad.Except
import Data.Vector (Vector, fromList, (!?), (//), (!))
import Debug.Trace

--------------------------------------------------------------------------------

type Parser s = StateT (PS s Char) (Either String)

newtype Pos = Pos { unPos :: (Int, Int) }
	deriving (Eq, Ord)

instance Show Pos where
	show (Pos (x, y)) = show y ++ "/" ++ show x

runParser
	:: Int
	-> Int
	-> String
	-> StateT (PS st Char) (Either String) b
	-> st
	-> Either String (b, PS st Char)
runParser x y s f initSt = runStateT f $ PS (Pos (x, y)) (mat s') initPos initSt
	where
	initPos = Pos (0, 1)
-- 	initSt = (initPos, [])
	s' = fromList $ fmap fromList $ lines s

-- skip one character in current direction without eating it
skip :: MonadState (PS s a) m => m ()
skip = get
	>>= \(PS {pos=Pos (x, y), dir=Pos (dx, dy)})
		-> setPos $ Pos (x + dx, y + dy)

data PS st a = PS
	{ pos :: Pos
	, chars :: Vector (Vector (Bool, a))
	, dir :: Pos --x, y increment
	, user :: st
	} deriving Show

while :: (a -> Bool) -> StateT (PS s a) (Either String) [a]
while p = peek >>= \case
	Value False c | p c -> eat >> (c:) <$> while p
	_ -> return []

data Peek a
	= End
	| Value Bool a
	deriving Show

peek :: MonadState (PS s a) m => m (Peek a)
peek = get >>= \PS{..} -> case at pos chars of
	Just (visited, v) -> pure $ Value visited v
	Nothing -> pure End

setPos :: MonadState (PS s a) m => Pos -> m ()
setPos p = modify $ \st -> st { pos = p }

trcm :: (Show a, Show b) => b -> StateT (PS s a) (Either String) ()
trcm x = do
	p <- gets pos
	w <- peek
	traceShowM (x, p, w)

withDir :: MonadState (PS s a) m => Pos -> m b -> m b
withDir d' f = do
	d <- gets pos
	modify $ \st -> st { dir = d' }
	y <- f
	modify $ \st -> st { dir = d }
	return y

string :: (Eq a, MonadError String m, MonadState (PS s a) m) => [a] -> m [a]
string = sequence . fmap char

satisfy :: (MonadError String m, MonadState (PS s a) m) => (a -> Bool) -> m a
satisfy p = current >>= \case
	c | p c -> eat
	_ -> throwError "DiagramParser.satisfy"

lookahead :: (MonadError String m, MonadState (PS s a) m) => (a -> Bool) -> m a
lookahead p = current >>= \case
	a | p a -> return a
	_ -> throwError "DiagramParser.lookahead"

char :: (Eq a, MonadError String m, MonadState (PS s a) m) => a -> m a
char c = satisfy (c==)

setDir :: MonadState (PS s a) m => Pos -> m ()
setDir dir' = modify $ \st -> st { dir = dir' }

eat :: (MonadError String m, MonadState (PS s a) m) => m a
eat = justEatDontMove <* (gets (unPos . dir) >>= movePossiblyBeyondInput)

--eat char at current position, don't move
justEatDontMove :: (MonadError String m, MonadState (PS s a) m) => m a
justEatDontMove = get >>= \PS{pos=pos@(Pos(x, y)), ..} -> case peekCharAt pos chars of
	Just c
		-> put (PS pos
			( chars // [(y, (chars ! y) // [(x, (True, c))])] )
			dir user)
		>> return c
	Nothing -> throwError "DiagramParser.justEatDontMove"

movePossiblyBeyondInput :: (MonadState (PS s a) m) => (Int, Int) -> m ()
movePossiblyBeyondInput (dx, dy)
	= modify $ \st@PS{pos=Pos(x, y)} -> st { pos = Pos(x + dx, y + dy) }

current :: (MonadError String m, MonadState (PS s a) m) => m a
current = peek >>= \case
	Value False c -> return c
	_ -> throwError "DiagramParser.current"

mat :: (Functor f, Functor g) => f (g b) -> f (g (Bool, b))
mat = fmap (fmap (\x -> (False, x)))

at :: Pos -> Vector (Vector b) -> Maybe b
at (Pos (x, y)) s = s !? y >>= (!? x)

peekCharAt :: Pos -> Vector (Vector (Bool, a)) -> Maybe a
peekCharAt p s = case at p s of
	Just (False, x) -> Just x
	_ -> Nothing
