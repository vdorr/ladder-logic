#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Range
import System.Environment

import Data.Word

import Language.Ladder.Interpreter
import Language.Ladder.Target

--------------------------------------------------------------------------------

serializeTests :: TestTree
serializeTests = testGroup "Serialize"
    [ testProperty "Roundtrip" prop_prog_trip
    ]

genInstructions :: Gen [ExtendedInstruction Int Word8 Word16]
genInstructions = Gen.list (Range.linear 0 100) genInstruction

genInstruction :: Gen (ExtendedInstruction Int Word8 Word16)
genInstruction
    = Gen.choice $ instructionTable (pure 0) (pure 0) (pure 0) (pure 0)

prop_prog_trip :: Property
prop_prog_trip =
    withTests 1000 . property $ do
        prog <- forAll genInstructions
        tripping prog
            programToByteString
            (Just . findLabels . byteStringToInstructions)

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests"
    [ serializeTests
    ]

main :: IO ()
main = defaultMain tests
