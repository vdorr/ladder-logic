#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Ladder.DiagramParser where

import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)
import Control.Monad hiding (fail)

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
newtype SFM s a = SFM { sfm :: s -> Either String (a, s) }

-- |Move in some direction from provided origin
type MoveToNext tok = (Int, (Int, Int)) -> Dg tok -> Either String (Dg tok)

-- |Parser state
data DgPState tok = DgPSt
    { psNext     :: MoveToNext tok -- ^select next token
    , psStr      :: Dg tok         -- ^input
    , psLastBite :: Maybe DgExt    -- ^position of last token eaten
    , psFocused  :: Bool           -- ^current focus of zp is actual parser current token
    }

instance Functor (SFM s) where
    fmap = ap . return

instance Applicative (SFM s) where
    pure = return
    (<*>) = ap

instance Monad (SFM s) where
    return a = SFM $ \s -> return (a, s)
    a >>= b = SFM $ \s -> do
        (y, s') <- sfm a s
        sfm (b y) s'

instance MonadFail (SFM s) where
    fail = SFM . const . Left

instance Alternative (SFM s) where
    empty = SFM $ const $ Left "alt empty"
    a <|> b = SFM $ \s -> --sfm a s <|> sfm b s
                either (const $ sfm b s) Right (sfm a s)

--------------------------------------------------------------------------------

move_ :: Int -> Int -> Dg a -> Either String (Dg a)
move_ ln co dg = maybe (Left here) return (move ln co dg)
-- move_ ln co dg = maybe (Left here) return (moveNotCursed ln co dg)

--FIXME should move only to direct neigbour
-- (by using single step to exact position it probably works already)
goRight, goDown, goUp, goLeft :: MoveToNext tok
goRight (ln, (_, co)) = move_ ln     (co+1)
goDown  (ln, (co, _)) = move_ (ln+1) co
goUp    (ln, (co, _)) = move_ (ln-1) co
goLeft  (ln, (co, _)) = move_ ln     (co-1)

--------------------------------------------------------------------------------

lastPos :: SFM (DgPState tok) DgExt
lastPos = psLastBite <$> get >>= maybe (fail here) return

applyDgp :: SFM (DgPState tok) a -> Dg tok -> Either String (a, DgPState tok)
applyDgp p dg = sfm p (DgPSt goRight dg Nothing True)

get :: SFM s s
get = SFM $ \s -> return (s, s)

put :: s -> SFM s ()
put s = SFM $ \_ -> return ((), s)

modify :: (s -> s) -> SFM s ()
modify f = f <$> get >>= put

setDir :: MoveToNext tok -> SFM (DgPState tok) ()
setDir f = modify $ \(DgPSt _ zp ps fc) -> DgPSt f zp ps fc

getDir :: SFM (DgPState tok) (MoveToNext tok)
getDir = psNext <$> get

step :: SFM (DgPState tok) ()
step = do
    origin             <- currentPos
    DgPSt f zp ps True <- get --if nothing is focused, currentPos makes no sense
    case f origin zp of
        Right zp' -> put (DgPSt f zp' ps True)
        Left err -> fail here --or not?

setPos :: (Int, (Int, b)) -> SFM (DgPState tok) ()
setPos (ln, (co, _)) = do
    DgPSt b zp ps _ <- get
    Just zp'        <- return $ move ln co zp --FIXME can only move to direct neighbour!!!!!!!
    put (DgPSt b zp' ps True)

--------------------------------------------------------------------------------

-- |Succeeds FIXME FIXME i am not sure when
end :: SFM (DgPState tok) ()
end = do
    Nothing <- (cursor . psStr) <$> get
    return ()

eat :: SFM (DgPState tok) tok
eat = do
    DgPSt nx dg ps True <- get
    case dgPop dg of
        Just (v, pos, dg') -> do
            put $ case nx pos dg' of
                Right q -> DgPSt nx q   (Just pos) True
                Left  _ -> DgPSt nx dg' (Just pos) False --nowhere to move
            return v
        Nothing -> fail $ show (here, ps)

currentPosM :: SFM (DgPState tok) (Maybe DgExt)
currentPosM = (pos . psStr) <$> get

currentPos :: SFM (DgPState tok) DgExt
currentPos = do
    Just p <- (pos . psStr) <$> get
    return p

peek :: SFM (DgPState tok) tok
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

cursor :: Dg a -> Maybe a
cursor (DgFocused x) = Just x
cursor _             = Nothing

-- |Match on current token position
pattern DgFocusedPos ln cl cr <- Zp _ ((ln, Zp _ (((cl, cr), x) : _)) : _)

pos :: Dg a -> Maybe (Int, (Int, Int))
pos (DgFocusedPos ln cl cr) = Just (ln, (cl, cr))
pos _                       = Nothing

mkDgZp :: [(Int, [((Int, Int), tok)])] -> Dg tok
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
