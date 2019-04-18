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

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests" [unitTests]

testPreproc4 :: Text -> Either Text [[Tok]]
testPreproc4 = fmap (fmap (snd . fmap (fmap snd))) . preproc4

testPreproc5 :: Text -> Either Text [Tok]
testPreproc5 = fmap (fmap ( snd)) . preproc5

unitTests = testGroup "Tokenize"
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

main :: IO ()
main = defaultMain tests
