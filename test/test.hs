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

unitTests = testGroup "Unit tests"
	[ testCase "tokenize 1" $
		(Right [[VLine]] @=?) $ testPreproc4 $ [text|
			|
			|]

-- 	, testCase "tokenize 2" $
-- 		(@?= Right []) $ testPreproc4  ""
	]

main :: IO ()
main = defaultMain tests
