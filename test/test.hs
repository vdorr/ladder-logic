{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import NeatInterpolation
import Data.Text (Text)

import Preprocess
import Zipper

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests" [tokenizerTests, zipperTests]

testPreproc4 :: Text -> Either Text [[Tok]]
testPreproc4 = fmap (fmap (snd . fmap (fmap snd))) . preproc4

testPreproc5 :: Text -> Either Text [Tok]
testPreproc5 = fmap (fmap ( snd)) . preproc5

tokenizerTests = testGroup "Tokenize"
	[ testCase "empty string" $
		(@?= Right []) $ testPreproc4  ""
	, testCase "one VLine" $
		(Right [[VLine]] @=?) $ testPreproc4 $ [text|
			|
			|]
	, testCase "3" $
		(Right [[VLine], [Node], [VLine]] @=?) $ testPreproc4 $ [text|
			| (* hello *)
			+
			|
			|]

	, testCase "4" $
		(Right [VLine, Node, VLine] @=?) $ testPreproc5 $ [text|
			| (* hello *)
			+
			|
			|]
	, testCase "5" $
		(Right [[VLine],[Node,HLine,Jump' "LBL"]] @=?) $ testPreproc4 $ [text|
			| (* hello *)
			+->>LBL
			|]
	]

zipperTests = testGroup "Zipper"
	[ testCase "from/to list" $
		zpToList <$> (stepRight $ zpFromList [1,2,3,4]) @=? Just [1,2,3,4]
	,  testCase "length" $
		zpLength <$> (stepRight $ zpFromList [1,2]) @=? Just 2
	,  testCase "bad move" $
		stepLeft (zpFromList [1::Int,2]) @=? Nothing
	]

main :: IO ()
main = defaultMain tests
