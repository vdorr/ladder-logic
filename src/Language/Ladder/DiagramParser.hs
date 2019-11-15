#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")
{-# LANGUAGE FlexibleInstances #-}

module Language.Ladder.DiagramParser where

import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Applicative
import Control.Monad hiding (fail)
import Data.Maybe
import Data.Traversable

import Control.Monad.Except hiding (fail)
import Control.Monad.State hiding (fail)

import Language.Ladder.Zipper

--------------------------------------------------------------------------------

-- |Returns number of remaining tokens
dgLength :: Dg a -> Int
dgLength (Zp l r) = sum (fmap zpLength l) + sum (fmap zpLength r)

dgNull :: Dg a -> Bool
dgNull = (>0) . dgLength --FIXME

-- |Drop empty lines
dgTrim :: Dg a -> Dg a
dgTrim (Zp l r) = Zp (trim l) (trim r)
    where
    trim = filter (not . zpNull)

--------------------------------------------------------------------------------

type SFM s = StateT s (Either String)

sfm :: SFM s a -> s -> Either String (a, s)
sfm = runStateT

instance Control.Monad.Fail.MonadFail (Either String) where
    fail = Left

--------------------------------------------------------------------------------

-- |Diagram parser input stream (or, say, input vortex)
type Dg a = Zp (Zp (DgExt, a))

-- |Token position and extent
type DgExt = (Int, (Int, Int))

--------------------------------------------------------------------------------

-- |Move in some direction from provided origin
type MoveToNext tok = DgExt -> Dg tok -> Either String (Dg tok)

--TODO TODO make some tests for psFocused behaviour (e.g. gap test)

-- |Parser state
data DgPState st lx = DgPSt
    { psNext     :: MoveToNext lx -- ^select next token
    , psStr      :: Dg lx         -- ^input
    , psLastBite :: Maybe DgExt   -- ^position of last token eaten
    , psFocused  :: Bool          -- ^current focus of zp is actual parser current token
    , psUser     :: !st 
    }

--------------------------------------------------------------------------------

applyDgp :: SFM (DgPState st tok) a -> Dg tok -> st -> Either String (a, DgPState st tok)
applyDgp p dg st = sfm p (DgPSt goRight dg Nothing True st)

--------------------------------------------------------------------------------

move_ :: Int -> Int -> Dg a -> Either String (Dg a)
move_ ln co dg = maybe (throwError here) return (move ln co dg)
-- move_ ln co dg = maybe (throwError here) return (moveNotCursed ln co dg)

--FIXME should move only to direct neigbour
-- (by using single step to exact position it probably works already)
goRight, goDown, goUp, goLeft :: MoveToNext tok
goRight (ln, (_, co)) = move_ ln     (co+1)
goDown  (ln, (co, _)) = move_ (ln+1) co
goUp    (ln, (co, _)) = move_ (ln-1) co
goLeft  (ln, (co, _)) = move_ ln     (co-1)

--------------------------------------------------------------------------------

lastPos :: SFM (DgPState st tok) DgExt
lastPos = psLastBite <$> get >>= maybe (fail here) return

setDir :: MoveToNext tok -> SFM (DgPState st tok) ()
setDir f = modify $ \(DgPSt _ zp ps fc st) -> DgPSt f zp ps fc st

-- getDir :: SFM (DgPState st tok) (MoveToNext tok)
-- getDir = psNext <$> get

step :: SFM (DgPState st tok) ()
step = do
    origin                <- currentPos
    DgPSt f zp ps True st <- get --if nothing is focused, currentPos makes no sense
    case f origin zp of
        Right zp' -> put (DgPSt f zp' ps True st)
        Left  _err -> fail here --or not?

setPos :: (Int, (Int, b)) -> SFM (DgPState st tok) ()
setPos (ln, (co, _)) = do
    DgPSt b zp ps _  st <- get
    Just zp'        <- return $ move ln co zp --FIXME can only move to direct neighbour!!!!!!!
    put (DgPSt b zp' ps True st)

-- |Clear 'psFocused' flag if there is not lexeme at desired position
-- used by vertical diagram combinators
setPosOrBlur :: (Int, (Int, b)) -> SFM (DgPState st tok) ()
setPosOrBlur (ln, (co, _)) = do
    DgPSt b zp ps _ st <- get
    let zp' = move ln co zp --FIXME can only move to direct neighbour!!!!!!!
    put $ case zp' of
        Just zp'' -> DgPSt b zp'' ps True  st
        Nothing   -> DgPSt b zp   ps False st

--------------------------------------------------------------------------------

eat :: SFM (DgPState st tok) tok
eat = do
    DgPSt nx dg ps True st <- get
    case dgPop dg of
        Just ((p, v), dg') -> do
            put $ case nx p dg' of
                Right q -> DgPSt nx q   (Just p) True  st
                Left  _ -> DgPSt nx dg' (Just p) False st --nowhere to move
            return v
        Nothing -> fail $ show (here, ps)

--------------------------------------------------------------------------------

-- |Succeeds FIXME FIXME i am not sure when
end :: SFM (DgPState st tok) ()
end = do
    Nothing <- (cursor . psStr) <$> get
    return ()

currentPosM :: SFM (DgPState st tok) (Maybe DgExt)
currentPosM = (pos . psStr) <$> get

currentPos :: SFM (DgPState st tok) DgExt
currentPos = do
    Just p <- currentPosM
    return p

--TODO implement in terms of 'peekM'
peek :: SFM (DgPState st tok) tok
peek = do
    Just p <- peekM
    return p

peekM :: SFM (DgPState st tok) (Maybe tok)
peekM = (cursor . psStr) <$> get

-- --for VLine crossing impl
-- skipSome :: (tok -> Bool) -> SFM (DgPState st tok) ()
-- skipSome f
--     = peekM >>= \case
--          Just x | f x -> step >> skipSome f
--          _            -> return ()
-- 
-- -- |Step over lexeme or do nothing
-- skip :: (tok -> Bool) -> SFM (DgPState st tok) ()
-- skip f
--     = peekM >>= \case
--          Just x | f x -> step
--          _            -> return ()
-- 
option :: SFM st a -> SFM st (Maybe a)
option p = (Just <$> p) <|> pure Nothing

--------------------------------------------------------------------------------

-- |Fail if input stream is not empty
dgIsEmpty :: SFM (DgPState st tok) ()
-- dgIsEmpty :: Show tok => SFM (DgPState st tok) ()
dgIsEmpty
    =   (dgNull . psStr) <$> get
    >>= (`when` fail (here ++ "not empty"))

{-
   |
   +-------
   |\
   | *-------
  
-}

branch
    :: (tok -> Bool)
    -> [(MoveToNext tok, SFM (DgPState st tok) a)]
    ->  SFM (DgPState st tok) [a]
branch isFork branches = do
    origin <- currentPos
    True   <- isFork <$> peek
    stuff  <- for branches $ \(dir, p) -> do
        setDir dir
        (setPos origin *> step *> (Just <$> p))
        <|> return Nothing --step fail if there's nothing in desired direction
    setPos origin --eat `fork`
--     setDir dir0 --restore direction, good for parsing boxes
    _ <- eat --FIXME set direction!!!!!!!!!!!!!
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
pattern DgLineEnd :: Zp (Zp a1)
pattern DgLineEnd <- Zp _l (Zp _ [] : _)

-- |Succeeds only when positioned on end of line
eol :: SFM (DgPState st tok) ()
eol = do
    psStr <$> get >>= \case
        DgLineEnd -> return ()
        _         -> fail here

colRight :: DgExt -> DgExt
colRight (ln, (_, co)) = (ln, (co + 1, co + 1))

colUnder :: DgExt -> DgExt
colUnder (ln, (_, co)) = (ln + 1, (co, co))

--------------------------------------------------------------------------------

test :: Dg a -> Maybe ((DgExt, a), Dg a)
test dg = pop dg
    >>= \(ln, dg') -> 
        fmap (fmap (fmap (dgTrim . push dg')))
            pop ln
--     >>= \(x, ln') -> 
--         return (x, dgTrim $ push dg' ln')

--Zp a -> (a -> Maybe a) -> Maybe (Zp a)

-- |Pop focused item, return its extent and updated zipper
dgPop :: Dg a -> Maybe ((DgExt, a), Dg a)
dgPop (Zp u ((Zp l (x : rs)) : ds))
    = Just (x, dgTrim $ Zp u ((Zp l rs) : ds))
dgPop _ = Nothing

-- pattern DgFocused :: b -> Zp (Zp (a1, b))
-- pattern DgFocused x <- Zp _ ((Zp _ ((_, x) : _)) : _)

cursor :: Dg a -> Maybe a
-- cursor (DgFocused x) = Just x
-- cursor _             = Nothing
cursor = tip >=> tip >=> pure . snd

-- |Match on current token position
-- pattern DgFocusedPos :: p -> Zp (Zp (p, a))
-- pattern DgFocusedPos p <- Zp _ (Zp _ ((p, _) : _) : _)

pos :: Dg a -> Maybe DgExt
-- pos (DgFocusedPos p) = Just p
-- pos _                = Nothing
-- pos = fmap fst . (tip >=> tip)
pos = tip >=> tip >=> pure . fst

-- line :: Dg a -> Maybe Int
-- line = pos >=> pure . fst

mkDgZp :: [(Int, [((Int, Int), tok)])] -> Dg tok
mkDgZp q= Zp [] $ (fmap (Zp [])) $ xxx q
    where
        xxx = fmap (\(nr, ln) -> fmap (\(co, tok) -> ((nr, co), tok)) ln)

--------------------------------------------------------------------------------

-- moveNotCursed :: Int -> Int -> Dg a -> Maybe (Dg a)
-- moveNotCursed line col = moveToLine line >=> moveToCol col

move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col = (moveToLine line >=> moveToCol col) . focusDg --FIXME get rid of focusing

--FIXME merge with moveToLine somehow?
moveToCol :: Int -> Dg a -> Maybe (Dg a)
moveToCol col (Zp us (zp : ds)) = reassemble <$> move2 (dir col . fst) zp
    where
    dir x (_, (a, b))
        | x < a = LT
        | x > b = GT
        | otherwise = EQ
    reassemble zp' = Zp us (zp' : ds)
moveToCol _ _ = Nothing

--FIXME get rid of focusing
focusDg :: Dg a -> Dg a
focusDg = fmap focus . focus

moveToLine :: Int -> Dg a -> Maybe (Dg a)
moveToLine ln = move2 (compare (Just ln) . lnNr)
    where
    lnNr = tip >=> pure . fst . fst
--     lnNr (Zp [] []                ) = Nothing
--     lnNr (Zp (((lnx, _), _) : _) _) = Just lnx
--     lnNr (Zp _ (((lnx, _), _) : _)) = Just lnx

--------------------------------------------------------------------------------

colocated :: (DgExt -> DgExt)
          -> SFM (DgPState st tok) t
          -> (t -> SFM (DgPState st tok) b1)
          -> SFM (DgPState st tok) b1
colocated mapPos p pp = do
    begin <- currentPos
    x     <- p
    next  <- currentPos
    _     <- setPosOrBlur (mapPos begin)
    y     <- pp x
    _     <- setPos next
    return y

colocated_
    :: (DgExt -> DgExt)
    -> SFM (DgPState st tok) a
    -> SFM (DgPState st tok) b
    -> SFM (DgPState st tok) (a, b)
colocated_ mapPos p pp = colocated mapPos p (\x -> (x,) <$> pp)

above, below
    :: SFM (DgPState st tok) t
    -> (t -> SFM (DgPState st tok) b)
    -> SFM (DgPState st tok) b

above = colocated (\(ln, co) -> (ln - 1, co))

below = colocated (\(ln, co) -> (ln + 1, co))

above_, below_
    :: SFM (DgPState st tok) a
    -> SFM (DgPState st tok) b
    -> SFM (DgPState st tok) (a, b)

above_ = colocated_ (\(ln, co) -> (ln - 1, co))

below_ = colocated_ (\(ln, co) -> (ln + 1, co))

--------------------------------------------------------------------------------
