{-# LANGUAGE OverloadedStrings #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Zipper where

import Prelude hiding (fail)
import Control.Monad.Fail
import Control.Applicative hiding (fail)
import Data.Foldable
import Data.Text (Text, unpack)
import Data.Bifunctor
import Control.Monad hiding (fail)

-- import Debug.Trace
-- import GHC.Stack
-- import GHC.Exts

-- import Ladder.Zipper
import Ladder.Lexer
import Ladder.DiagramParser
import Ladder.LadderParser
