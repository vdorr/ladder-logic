{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import NeatInterpolation
import Data.Text (Text)
import qualified Data.Text
import Data.Bifunctor

import Preprocess
import Zipper

--------------------------------------------------------------------------------

testPreproc4 :: Text -> Either Text [[Tok]]
testPreproc4 = fmap (fmap (snd . fmap (fmap snd))) . preproc4

testPreproc5 :: Text -> Either Text [Tok]
testPreproc5 = fmap (fmap ( snd)) . preproc5

tokenizerTests = testGroup "Tokenizer"
	[ testCase "empty string" $
		(@?= Right []) $ testPreproc4  ""
	, testCase "one VLine" $
		(Right [[VLine]] @=?) $ testPreproc4 $ [text|
			|               |]
	, testCase "3" $
		(Right [[VLine], [Node], [VLine]] @=?) $ testPreproc4 $ [text|
			| (* hello *)
			+
			|               |]
	, testCase "4" $
		(Right [VLine, Node, VLine] @=?) $ testPreproc5 $ [text|
			| (* hello *)
			+
			|               |]
	, testCase "5" $
		(Right [[VLine],[Node,HLine,Jump' "LBL"]] @=?) $ testPreproc4 $ [text|
			| (* hello *)
			+->>LBL         |]
	, testCase "label" $
		(Right [[VLine],[Label' "LBL"],[VLine]] @=?) $ testPreproc4 $ [text|
			| (* hello *)
			LBL:
			|               |]
	, testCase "continuation" $
		(Right [[VLine],[Node,HLine,Continuation "LBL"],[VLine]] @=?)
			$ testPreproc4 $ [text|
				| (* hello *)
				+-->LBL>
				|               |]
	, testCase "return" $
		(Right [[VLine],[Node,HLine,Return],[VLine]] @=?)
			$ testPreproc4 $ [text|
				| (* hello *)
				+--<RETURN>
				|               |]



	, testCase "invalid char" $
		 simpleResult (testPreproc4 "?") @?= Left True
	]

simpleResult = bimap ((>0).Data.Text.length) id

zipperTests = testGroup "Zipper"
	[ testCase "from/to list" $
		zpToList <$> (stepRight $ zpFromList [1,2,3,4]) @=? Just [1,2,3,4]
	,  testCase "length" $
		zpLength <$> (stepRight $ zpFromList [0,1,2,3]) @=? Just 4
	,  testCase "bad move" $
		stepLeft (zpFromList [1::Int,2]) @=? Nothing
	,  testCase "okay move" $
		(stepRight (zpFromList [1,2]) >>= tip) @=? Just 2
	,  testCase "move there and back" $
		(stepRight (zpFromList [1,2]) >>= stepLeft >>= tip) @=? Just 1
	,  testCase "okay focus" $
		tip (foc (Zp [2,1] [])) @=? Just 2
	,  testCase "nothing to focus to" $
		tip (foc (zpFromList [])) @=? (Nothing :: Maybe ())
	]

dgpTests = testGroup "Diagram parser"
	[ testCase "length" $
		dgLength (mkDgZp []) @=? 0
	, testCase "trivial" $
		fst <$> applyDgp (pure ()) (mkDgZp []) @=? Right ()
	, testCase "trim 1" $
		dgTrim (Zp [] [(1, Zp [] [])]) @=? mkDgZp []
	, testCase "trim 2" $
		dgTrim (Zp [] [(1, Zp [] [((1, 1), VLine)])])
			@=? mkDgZp [(1, [((1, 1), VLine)])]
	]

ladderTests = testGroup "Ladder parser"
	[ testCase "test01" $
		fmap (dgTrim.psStr.snd) (applyDgp test002 (mkDgZp t01))
			@?= Right (Zp [] [])
	, testCase "test04" $
		fmap (dgTrim.psStr.snd) (applyDgp test002 (mkDgZp t04))
			@?= Right (Zp [] [])
	, testCase "test07a" $
		fmap (dgTrim.psStr.snd) (applyDgp test002 (mkDgZp t07a))
			@?= Right (Zp [] [])
-- 	, testCase "test07" $
-- 		fmap (dgTrim.psStr.snd) (applyDgp test002 (mkDgZp t07))
-- 			@?= Right (Zp [] [])
	]
	where
	Right t01 = test01_tokenized
	Right t04 = test04_tokenized
	Right t07 = test07_tokenized
	Right t07a = test07a_tokenized

test01_tokenized = preproc4'' test01
test04_tokenized = preproc4'' test04
test07_tokenized = preproc4'' test07
test07a_tokenized = preproc4'' test07a

test01 =
	[text|
	(* --- test 01 --- *)

	| %IX0  %QX0
	+--[ ]---( )--
	|                          |]

test04 =
	[text|
	(* --- test 04 --- *)

	| %MX0 %MX1      %MX1
	+--[/]--[/]-------(S)-
	| %MX0 %MX1 %MX0 %MX1
	+--[/]--[ ]--(S)--(R)-
	| %MX0 %MX1      %MX1
	+--[ ]--[/]-------(S)-
	| %MX0 %MX1 %MX0 %MX1
	+--[ ]--[ ]--(R)--(R)-
	|                          |]

test07 =
	[text|
	(* --- test 07 --- *)

	| %IX0    %QX0
	+--[ ]--+--( )--
	| %IX1  |
	+--[ ]--+
	|                          |]

test07a =
	[text|
	(* --- test 07 --- *)

	|    %QX0
	+--+--( )--
	|                          |]


boxTests = testGroup "Box parser"
	[ testCase "1" $
		fmap (dgTrim.psStr.snd) (applyDgp (box001 2) (mkDgZp box01_tokenized))
			@?= Right (Zp [] [])
	]
	where
	Right box01_tokenized = preproc4'' box01

box01 =
	[text|
	+-+
	| |
	+-+                        |]

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Tests"
	[ tokenizerTests
	, zipperTests
	, dgpTests
	, ladderTests
	, boxTests
	]

main :: IO ()
main = defaultMain tests
