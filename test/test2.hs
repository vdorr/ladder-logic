#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Range
import System.Environment

import Language.Ladder.Target

main = return ()
