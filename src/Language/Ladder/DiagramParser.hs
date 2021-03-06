
module Language.Ladder.DiagramParser
    ( colocated'', colocated_'', above, below, above_, below_
    , ParsingDirection
    , DgPState -- (..) --FIXME do not export
    , DgExt, Dg, SFM
    , gap, end, eat, eol
    , dgIsEmpty --FIXME better name
    , setDir
    , setPos
    , bridge
    , currentPos, lastPos, keepOrigin
    , goRight, goDown, goUp, goLeft
    , goRight'', goDown'', goUp'', goLeft''
--     , colRight, colUnder --XXX remove
    , applyDgp, mkDgZp
    , dgLength, dgTrim
    , getState, getStream
    , failure
    , lookAround, nextPos
    )
    where

import Prelude hiding (fail)
-- import Control.Monad.Fail
-- import Control.Applicative
import Control.Monad hiding (fail)
-- import Data.Maybe
-- import Data.Traversable
import GHC.Exts
import Control.Monad.Except hiding (fail)
import Control.Monad.State hiding (fail)

import Language.Ladder.Zipper
import Language.Ladder.Types

--------------------------------------------------------------------------------

-- |Returns number of remaining tokens
dgLength :: Dg a -> Int
dgLength = sum . fmap length

-- | null or not null idontknow
-- dgNull :: Dg a -> Bool
-- dgNull = or . fmap (not . null)

-- |Drop empty lines
dgTrim :: Dg a -> Dg a
dgTrim = zpFilter (not . null)

--------------------------------------------------------------------------------

type SFM s = StateT s (Either String)

sfm :: SFM s a -> s -> Either String (a, s)
sfm = runStateT

--------------------------------------------------------------------------------

-- |Diagram parser input stream (or, say, input vortex)
type Dg a = Zp (Zp (DgExt, a))

--------------------------------------------------------------------------------

-- |Move in some direction from provided origin
type ParsingDirection tok = DgExt -> Dg tok -> Either (DgExt, String) (Dg tok)

--TODO TODO make some tests for psFocused behaviour (e.g. gap test)

-- |Parser state
data DgPState st lx = DgPSt
    { psNext     :: ParsingDirection lx -- ^select next token
    , psStr      :: Dg lx         -- ^input
    , psLastBite :: Maybe DgExt   -- ^position of last token eaten
    , psFocused  :: Bool          -- ^current focus of zp is actual parser current token
    , psUser     :: !st 
    }

--------------------------------------------------------------------------------

applyDgp :: SFM (DgPState st tok) a -> Dg tok -> st -> Either String (a, DgPState st tok)
applyDgp p dg st = sfm p (DgPSt goRight dg Nothing True st)

--------------------------------------------------------------------------------

lookAround :: SFM (DgPState st tok) a -> SFM (DgPState st tok) a
lookAround p
    = gets (sfm p) >>= \case
         Right (a, _) -> return a
         Left   err   -> failure err

-- |expected position of next token
nextPos :: SFM (DgPState st tok) DgExt
nextPos = do
    p <- lastPos
    next <- gets psNext
    str <- gets psStr
    case next p str of
         Left (x, _)                     -> return x
         Right str' | Just x <- pos str' -> return x
         _                               -> failure "nextPos"

--------------------------------------------------------------------------------

move_ :: Int -> Int -> Dg a -> Either (DgExt, String) (Dg a)
move_ ln co dg = maybe (Left ((ln, (co, co)), "move_: bad move")) Right (move ln co dg)
-- move_ ln co dg = maybe (throwError here) return (moveNotCursed ln co dg)

--FIXME should move only to direct neigbour
-- (by using single step to exact position it probably works already)
goRight, goDown, goUp, goLeft :: ParsingDirection tok
goRight (ln, (_, co)) = move_ ln     (co+1)
goDown  (ln, (co, _)) = move_ (ln+1) co
goUp    (ln, (co, _)) = move_ (ln-1) co
goLeft  (ln, (co, _)) = move_ ln     (co-1)

--------------------------------------------------------------------------------

failure :: String -> SFM (DgPState st tok) a
failure err = lift $ Left err --TODO add location if possible

getState :: SFM (DgPState st tok) st
getState = gets psUser

getStream :: SFM (DgPState st tok) (Dg tok)
getStream = gets psStr

lastPos :: SFM (DgPState st tok) DgExt
lastPos = psLastBite <$> get >>= maybe (failure "lastPos: nothing parsed") pure

setDir :: ParsingDirection tok -> SFM (DgPState st tok) ()
setDir f = modify $ \(DgPSt _ zp ps fc st) -> DgPSt f zp ps fc st

-- step :: SFM (DgPState st tok) ()
-- step = do
--     origin                <- currentPos
--     DgPSt f zp ps focused st <- get --if nothing is focused, currentPos makes no sense
--     guard focused
--     case f origin zp of
--         Right zp'  -> put (DgPSt f zp' ps True st)
--         Left  _err -> lift $ Left here --or not?

setPos :: (Int, (Int, b)) -> SFM (DgPState st tok) ()
setPos = setPosOrBlur

-- |Clear 'psFocused' flag if there is not lexeme at desired position
-- used by vertical diagram combinators
setPosOrBlur :: (Int, (Int, b)) -> SFM (DgPState st tok) ()
setPosOrBlur (ln, (co, _)) = do
    DgPSt b zp ps _ st <- get
    let zp' = move ln co zp --FIXME can only move to direct neighbour!!!!!!!
    put case zp' of
        Just zp'' -> DgPSt b zp'' ps True  st
        Nothing   -> DgPSt b zp   ps False st

bridge :: Int -> SFM (DgPState st tok) ()
bridge 0 = pure ()
bridge vl = do
    (ln, (co, _)) <- currentPos
    setPos (ln, (co + vl, ()))--TODO TEST move to same location is noop

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
        Nothing -> failure $ "eat: " ++ show ps

--------------------------------------------------------------------------------

gap :: SFM (DgPState st tok) ()
gap = do
    get >>= \case
        DgPSt{psFocused=False} -> return ()
        DgPSt{psStr=DgLineEnd} -> return ()
        _         -> failure "gap: no gap"

-- |Succeeds FIXME FIXME i am not sure when
end :: SFM (DgPState st tok) ()
end = (cursor . psStr) <$> get >>= \case
                                         Nothing -> return ()
                                         _ -> failure "not empty"

currentPosM :: SFM (DgPState st tok) (Maybe DgExt)
currentPosM = (pos . psStr) <$> get

currentPos :: SFM (DgPState st tok) DgExt
currentPos = currentPosM >>= \case
                       Just p -> return p
                       _ -> failure "empty"

--------------------------------------------------------------------------------

-- |Fail if input stream is not empty
dgIsEmpty :: SFM (DgPState st tok) ()
dgIsEmpty
    = fmap focusDg getStream >>= \case
                                    (Zp _ (Zp _ ((p, _):_):_) ) -> 
                                        failure $ "dgIsEmpty: not empty " ++ show p
                                    _ -> return ()
{-
   |
   +-------
   |\
   | *-------
  
-}

goLeft'', goUp'', goDown'', goRight'' :: SFM (DgPState st tok) ()
goLeft'' = step'' goLeft
goUp'' = step'' goUp
goDown'' = step'' goDown
goRight'' = step'' goRight

step'' :: ParsingDirection tok -> SFM (DgPState st tok) ()
step'' f = do
    origin <- gets psLastBite >>= \case
                                    Just ppp -> return ppp
                                    _ -> undefined --use currently focused position?
    DgPSt _ zp ps _focused st <- get
    put case f origin zp of
            Right zp' -> DgPSt f zp' ps True  st
            Left  _   -> DgPSt f zp  ps False st

-- backup and restore last bit position for parsing multiple paths from same origin
keepOrigin :: SFM (DgPState st tok) a -> SFM (DgPState st tok) a
keepOrigin p = do
    origin <- gets psLastBite
    x      <- p
    _      <- modify \st -> st { psLastBite = origin }
    return x

-- -- set last bit so whole extent of parsed sequence
-- group :: SFM (DgPState st tok) a -> SFM (DgPState st tok) a
-- group p = do
--     first <- currentPos
--     x <- p
--     lastToken <- gets psLastBite
--     modify \st -> st { psLastBite = ext first <$> lastToken }
--     return x
-- 
-- ext :: DgExt -> DgExt -> DgExt
-- ext (a, (b, c)) (_d, (e, f))
--     = (a, (min b e, max c f))

-- branch
--     :: (tok -> Bool)
--     -> [(ParsingDirection tok, SFM (DgPState st tok) a)]
--     ->  SFM (DgPState st tok) [a]
-- branch isFork branches = do
--     gets psFocused >>= guard
--     origin <- currentPos
--     fmap isFork peek >>= (`unless` (lift $ Left $ here ++ "not branch"))
--     stuff  <- for branches $ \(dir, p)
--         -> optional (setDir dir *> setPos origin *> step *> p)
-- --         <|> return Nothing --step fail if there's nothing in desired direction
--     setPos origin --eat `fork`
-- --     setDir dir0 --restore direction, good for parsing boxes
--     _ <- eat --FIXME set direction!!!!!!!!!!!!!
--     return $ catMaybes stuff

-- |Matches diagram with nothing remaining on current line
pattern DgLineEnd :: Zp (Zp a1)
pattern DgLineEnd <- Zp _l (Zp _ [] : _)

-- |Succeeds only when positioned on end of line
eol :: SFM (DgPState st tok) ()
eol = do
    psStr <$> get >>= \case
        DgLineEnd -> return ()
        _         -> failure "eol: not eol"

-- colRight :: DgExt -> DgExt
-- colRight (ln, (_, co)) = (ln, (co + 1, co + 1))
-- 
-- colUnder :: DgExt -> DgExt
-- colUnder (ln, (_, co)) = (ln + 1, (co, co))

--------------------------------------------------------------------------------

-- |Pop focused item, return its extent and updated zipper
dgPop :: Dg a -> Maybe ((DgExt, a), Dg a)
dgPop dg = pop dg
    >>= \(ln, dg') -> 
        fmap (fmap (fmap (dgTrim . push dg')))
            pop ln

cursor :: Dg a -> Maybe a
cursor = tip >=> tip >=> pure . snd

pos :: Dg a -> Maybe DgExt
pos = tip >=> tip >=> pure . fst

mkDgZp :: [[(DgExt, tok)]] -> Dg tok
mkDgZp = fromList . fmap fromList

--------------------------------------------------------------------------------

move :: Int -> Int -> Dg a -> Maybe (Dg a)
move line col = (moveToLine line >=> moveToCol col) . focusDg --FIXME get rid of focusing

--FIXME merge with moveToLine somehow?
moveToCol :: Int -> Dg a -> Maybe (Dg a)
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

colocated'' :: SFM (DgPState st tok) a
          -> (a -> SFM (DgPState st tok) b) -- `option (goDown' varName)` or `goUp' argument`
          -> SFM (DgPState st tok) b
colocated'' p pp = do
    directionBackup <- gets psNext
    x     <- p
    next  <- currentPos
    lastBiteBackup  <- gets psLastBite
    y <- pp x
    modify \st -> st { psNext = directionBackup, psLastBite = lastBiteBackup }
    _     <- setPosOrBlur next
    return y

colocated_'' :: SFM (DgPState st tok) t
                      -> StateT (DgPState st tok) (Either String) t1
                      -> SFM (DgPState st tok) (t, t1)
colocated_'' p pp = colocated'' p (\x -> (x,) <$> pp)

-- colocated :: (DgExt -> DgExt)
--           -> SFM (DgPState st tok) t
--           -> (t -> SFM (DgPState st tok) b1)
--           -> SFM (DgPState st tok) b1
-- colocated mapPos p pp = do
--     begin <- currentPos
--     x     <- p
--     next  <- currentPos
--     _     <- setPosOrBlur (mapPos begin)
--     y     <- pp x
--     _     <- setPos next
--     return y
-- 
-- colocated_
--     :: (DgExt -> DgExt)
--     -> SFM (DgPState st tok) a
--     -> SFM (DgPState st tok) b
--     -> SFM (DgPState st tok) (a, b)
-- colocated_ mapPos p pp = colocated mapPos p (\x -> (x,) <$> pp)

above, below
    :: SFM (DgPState st tok) t
    -> (t -> SFM (DgPState st tok) b)
    -> SFM (DgPState st tok) b

-- above = colocated (\(ln, co) -> (ln - 1, co))
-- below = colocated (\(ln, co) -> (ln + 1, co))
above p pp = colocated'' p (\x -> goUp'' *> pp x)
below p pp = colocated'' p (\x -> goDown'' *> pp x)

above_, below_
    :: SFM (DgPState st tok) a
    -> SFM (DgPState st tok) b
    -> SFM (DgPState st tok) (a, b)

-- above_ = colocated_ (\(ln, co) -> (ln - 1, co))
-- below_ = colocated_ (\(ln, co) -> (ln + 1, co))
above_ p pp = colocated_'' p (goUp'' *> pp)
below_ p pp = colocated_'' p (goDown'' *> pp)

--------------------------------------------------------------------------------
