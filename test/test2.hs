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
import Text.Read

import Language.Ladder.Interpreter
import Language.Ladder.Target

--------------------------------------------------------------------------------

newtype Prog word address = Prog { unProg :: [ExtendedInstruction Int (Instruction word address)]}
    deriving (Show)

instance (Eq word, Eq address) => Eq (Prog word address) where
    Prog a == Prog b
        | la == lb + 1, (a', [EISimple ITrap]) <- splitAt lb a = a' == b
        | lb == la + 1, (b', [EISimple ITrap]) <- splitAt la b = b' == a
        | otherwise = a == b
        where
        la = length a
        lb = length b

serializeTests :: TestTree
serializeTests = testGroup "Serialize"
    [ testProperty "Single instruction roundtrip" prop_instr_trip
    , testProperty "Instruction list roundtrip" prop_prog_trip
--     , testProperty "Show/Read roundtrip" prop_show_trip
--     , testCase "1" $
--         read ("EIReturn")
--             @?= (EIReturn :: ExtendedInstruction Int Int Int)
    , testCase "2" $
        "EIReturn"
            @?= show (EIReturn :: ExtendedInstruction Int (Instruction Int Int))
    , testCase "3" $
        "IGt"
            @?= show (IGt :: Instruction Int Int)
    ]

genInstructions
    :: (Num word, Num address)
    => Gen [ExtendedInstruction Int (Instruction word address)]
genInstructions = Gen.list (Range.linear 0 100) genInstruction

genInstruction
    :: (Num word, Num address)
    => Gen (ExtendedInstruction Int (Instruction word address))
genInstruction
    = Gen.choice $ instructionTable (pure 0) (pure 0) (pure 0) (pure 0)

-- prop_show_trip :: Property
-- prop_show_trip
--     = withTests 1000 . property $ do
--         instr :: ExtendedInstruction Int Int Int <- forAll genInstruction
--         tripping instr
--             show
--             readMaybe

prop_instr_trip :: Property
prop_instr_trip
    = withTests 1000 . property $ do
        instr <- forAll genInstruction
        tripping instr
            (programToByteString . (:[]))
            (singleInstr . findLabels . byteStringToInstructions)
    where
    singleInstr [i, EISimple ITrap] = Right i --account for padding
    singleInstr [i                ] = Right i
    singleInstr other               = Left other


prop_prog_trip :: Property
prop_prog_trip
    = withTests 1000 . property $ do
        prog <- forAll genInstructions
        tripping (Prog prog)
            (programToByteString . unProg)
            (Just . Prog . findLabels . byteStringToInstructions)

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests"
    [ serializeTests
    ]

main :: IO ()
main = defaultMain tests
