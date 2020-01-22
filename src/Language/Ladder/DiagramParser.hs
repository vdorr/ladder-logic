#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")
{-# LANGUAGE FlexibleInstances #-}

module Language.Ladder.DiagramParser where

import Prelude hiding (fail)
-- import Control.Monad.Fail
import Control.Applicative
import Control.Monad hiding (fail)
import Data.Maybe
import Data.Traversable
import GHC.Exts

import Control.Monad.Except hiding (fail)
import Control.Monad.State hiding (fail)

import Language.Ladder.Zipper

--------------------------------------------------------------------------------

-- |Returns number of remaining tokens
dgLength :: Dg a -> Int
dgLength = sum . fmap length

-- | null or not null idontknow
dgNull :: Dg a -> Bool
-- dgNull = and . fmap null
dgNull = or . fmap (not . null)

-- |Drop empty lines
dgTrim :: Dg a -> Dg a
dgTrim = zpFilter (not . null)

--------------------------------------------------------------------------------

type SFM s = StateT s (Either String)

sfm :: SFM s a -> s -> Either String (a, s)
sfm = runStateT

-- instance Control.Monad.Fail.MonadFail (Either String) where
--     fail = Left

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
lastPos = psLastBite <$> get >>= maybe (lift $ Left here) return

setDir :: MoveToNext tok -> SFM (DgPState st tok) ()
setDir f = modify $ \(DgPSt _ zp ps fc st) -> DgPSt f zp ps fc st

-- getDir :: SFM (DgPState st tok) (MoveToNext tok)
-- getDir = psNext <$> get

step :: SFM (DgPState st tok) ()
step = do
    origin                <- currentPos
    DgPSt f zp ps focused st <- get --if nothing is focused, currentPos makes no sense
    guard focused
    case f origin zp of
        Right zp'  -> put (DgPSt f zp' ps True st)
        Left  _err -> lift $ Left here --or not?

setPos :: (Int, (Int, b)) -> SFM (DgPState st tok) ()
setPos = setPosOrBlur
-- setPos (ln, (co, _)) = do
--     DgPSt b zp ps _  st <- get
--     Just zp'        <- return $ move ln co zp --FIXME can only move to direct neighbour!!!!!!!
--     put (DgPSt b zp' ps True st)

-- |Clear 'psFocused' flag if there is not lexeme at desired position
-- used by vertical diagram combinators
setPosOrBlur :: (Int, (Int, b)) -> SFM (DgPState st tok) ()
setPosOrBlur (ln, (co, _)) = do
    DgPSt b zp ps _ st <- get
    let zp' = move ln co zp --FIXME can only move to direct neighbour!!!!!!!
    put case zp' of
        Just zp'' -> DgPSt b zp'' ps True  st
        Nothing   -> DgPSt b zp   ps False st

--------------------------------------------------------------------------------

eat :: SFM (DgPState st tok) tok
eat = do
    DgPSt next dg ps focused st <- get
    guard focused
    case dgPop dg of
        Just ((p, v), dg') -> do
            put case next p dg' of
                Right q -> DgPSt next q   (Just p) True  st
                Left  _ -> DgPSt next dg' (Just p) False st --nowhere to move
            return v
        Nothing -> lift $ Left $ show (here, ps)

--------------------------------------------------------------------------------

gap :: SFM (DgPState st tok) ()
gap = do
    get >>= \case
        DgPSt{psFocused=False} -> return ()
        DgPSt{psStr=DgLineEnd} -> return ()
        _         -> lift $ Left $ here

-- |Succeeds FIXME FIXME i am not sure when
end :: SFM (DgPState st tok) ()
-- end = void peek <|> (lift $ Left "not empty")
end = (cursor . psStr) <$> get >>= \case
                                         Nothing -> return ()
                                         _ -> lift $ Left "not empty"

currentPosM :: SFM (DgPState st tok) (Maybe DgExt)
currentPosM = (pos . psStr) <$> get

currentPos :: SFM (DgPState st tok) DgExt
currentPos = currentPosM >>= \case
                       Just p -> return p
                       _ -> lift $ Left "empty"

--TODO implement in terms of 'peekM'
peek :: SFM (DgPState st tok) tok
peek = peekM >>= \case
                       Just p -> return p
                       _ -> lift $ Left "empty"

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

--------------------------------------------------------------------------------

-- |Fail if input stream is not empty
dgIsEmpty :: SFM (DgPState st tok) ()
-- dgIsEmpty :: Show tok => SFM (DgPState st tok) ()
dgIsEmpty
--     =   (dgNull . psStr) <$> get
--     >>= (`when` fail (here ++ "not empty"))
--     = fmap (not . dgNull . psStr) get >>= guard
    =   (dgNull . psStr) <$> get
    >>= (`when` (lift $ Left $ here ++ "not empty"))

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
    gets psFocused >>= guard
    origin <- currentPos
    fmap isFork peek >>= (`unless` (lift $ Left $ here ++ "not branch"))
    stuff  <- for branches $ \(dir, p)
        -> optional (setDir dir *> setPos origin *> step *> p)
--         <|> return Nothing --step fail if there's nothing in desired direction
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
        _         -> lift $ Left $ here

colRight :: DgExt -> DgExt
colRight (ln, (_, co)) = (ln, (co + 1, co + 1))

colUnder :: DgExt -> DgExt
colUnder (ln, (_, co)) = (ln + 1, (co, co))

--------------------------------------------------------------------------------

-- |Pop focused item, return its extent and updated zipper
dgPop :: Dg a -> Maybe ((DgExt, a), Dg a)
dgPop dg = pop dg
    >>= \(ln, dg') -> 
        fmap (fmap (fmap (dgTrim . push dg')))
            pop ln
-- dgPop :: Dg a -> Maybe ((DgExt, a), Dg a)
-- dgPop (Zp u ((Zp l (x : rs)) : ds))
--     = Just (x, dgTrim $ Zp u ((Zp l rs) : ds))
-- dgPop _ = Nothing

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

-- mkDgZp :: [(Int, [((Int, Int), tok)])] -> Dg tok
-- mkDgZp q= Zp [] $ (fmap (Zp [])) $ xxx q
mkDgZp :: [[(DgExt, tok)]] -> Dg tok
mkDgZp = fromList . fmap fromList

--------------------------------------------------------------------------------

-- moveNotCursed :: Int -> Int -> Dg a -> Maybe (Dg a)
-- moveNotCursed line col = moveToLine line >=> moveToCol col

move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col = (moveToLine line >=> moveToCol col) . focusDg --FIXME get rid of focusing

--FIXME merge with moveToLine somehow?
moveToCol :: Int -> Dg a -> Maybe (Dg a)
-- moveToCol col (Zp us (zp : ds)) = reassemble <$> move2 (dir col . fst) zp
--     where
--     dir x (_, (a, b))
--         | x < a = LT
--         | x > b = GT
--         | otherwise = EQ
--     reassemble zp' = Zp us (zp' : ds)
-- moveToCol _ _ = Nothing
moveToCol col dg = pop dg >>= \(ln, dg') -> push dg' <$> move2 dir ln
    where
    dir ((_, (a, b)), _)
        | col < a   = LT
        | col > b   = GT
        | otherwise = EQ

--FIXME get rid of focusing
focusDg :: Dg a -> Dg a
focusDg = fmap focus . focus

moveToLine :: Int -> Dg a -> Maybe (Dg a)
moveToLine ln = move2 (compare (Just ln) . getLineNumber)
    where
    getLineNumber = tip >=> pure . fst . fst --extract line number

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
