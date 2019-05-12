
module Ladder.DiagramParser where

import Ladder.Zipper

import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)
-- import Data.Traversable
-- import Data.Foldable
-- import Data.Text (Text, unpack)
-- import Data.Bifunctor
-- import Data.Maybe
import Control.Monad hiding (fail)

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
