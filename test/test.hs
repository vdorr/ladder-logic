{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeApplications #-}

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.List
import Data.Ord
import Control.Monad

import NeatInterpolation
import Data.Text (Text, pack, unpack)
import qualified Data.Text
import Data.Bifunctor
import GHC.Exts

import Preprocess

-- import qualified LadderParser (Symbol_(..))
-- import LadderParser (Cofree(..), Symbol_(End, Source))
-- import DiagramParser (Pos(..))

import Ladder.Zipper
import Ladder.Lexer
import Ladder.DiagramParser
import Ladder.LadderParser

--------------------------------------------------------------------------------

preproc4'' :: Text -> Either Text [(Int, [((Int, Int), Tok Text)])]
preproc4'' = fmap stripPos . preproc5'

preproc5'' :: Text -> Either String [(Int, [((Int, Int), Tok Text)])]
preproc5'' = bimap unpack stripPos . preproc5'

testPreproc4 :: Text -> Either Text [[(Tok Text)]]
testPreproc4 = fmap (fmap (snd . fmap (fmap snd))) . preproc5'

testPreproc5 :: Text -> Either Text [Tok Text]
testPreproc5 = fmap concat . testPreproc4

--keep comments and pragmas
testPreproc6 :: Text -> Either Text [[(Tok Text)]]
testPreproc6 = fmap (fmap (snd . fmap (fmap snd))) . runLexer

--keep comments and pragmas
testLexer :: Text -> Either Text [Tok Text]
testLexer = fmap concat . testPreproc6

tokenizerTests = testGroup "Tokenizer"
    [ testCase "empty string" $
        (@?= Right []) $ testPreproc4  ""
    , testCase "one VLine" $
        (Right [[VLine]] @=?) $ testPreproc4 $ [text|
            |               |]
    , testCase "3" $
        (Right [[VLine], [Cross], [VLine]] @=?) $ testPreproc4 $ [text|
            | (* hello *)
            +
            |               |]
    , testCase "4" $
        (Right [VLine, Cross, VLine] @=?) $ testPreproc5 $ [text|
            | (* hello *)
            +
            |               |]
    , testCase "5" $
        (Right [[VLine],[Cross,HLine 0 0,Jump' "LBL"]] @=?) $ testPreproc4 $ [text|
            | (* hello *)
            +->>LBL         |]
    , testCase "label" $
        (Right [[VLine],[Label "LBL"],[VLine]] @=?) $ testPreproc4 $ [text|
            | (* hello *)
            LBL:
            |               |]
    , testCase "label/blocks" $
        (Right [(Nothing, [[VLine]]),(Just "LBL", [[VLine]])] @=?)
            $ fmap basicBlocks $ testPreproc4 $ [text|
                | (* hello *)
                LBL:
                |               |]
    , testCase "continuation" $
        (Right [[VLine],[Cross,HLine 1 0,Continuation "X"],[VLine]] @=?)
            $ testPreproc4 $ [text|
                | (* hello *)
                +-->X>
                |               |]
    , testCase "return" $
        (Right [[VLine],[Cross,HLine 1 0,Return],[VLine]] @=?)
            $ testPreproc4 $ [text|
                | (* hello *)
                +--<RETURN>
                |               |]
    , testCase "connector" $
        (Right [[Continuation "X", HLine 1 0]] @=?)
            $ testPreproc4 $ [text|
                (* hello *)
                >X>--           |]

    , testCase "device" $
        (Right [[VLine,Name "a",Name "b"],[Cross,HLine 1 0,Contact " ",HLine 1 0,Coil "/",HLine 1 0]]
            @=?)
            $ testPreproc4 $ [text|
                |  a    b
                +--[ ]--(/)--   |]

    , testCase "negation" $
        (Right [[VLine],[Cross,HLine 1 0,Number 0]]
            @=?)
            $ testPreproc4 $ [text|
            |
            +--0        |]

    , testCase "invalid char" $
        simpleResult (testPreproc4 "?") @?= Left True

    , testCase "ehmm" $
        show (fmap id (Contact ())) @?= "Contact ()"

    , testProperty "lexeme roundtrip" prop_lexeme_trip
--     , testProperty "lexer roundtrip" prop_trip

    ]

prop_lexeme_trip :: Property
prop_lexeme_trip =
    withTests 1000 . property $ do
        tok <- forAll genToken
        tripping [tok]
            (pack . foldMap (renderLexeme . fmap unpack))
            testLexer

--TODO
prop_trip :: Property
prop_trip =
    withTests 1000 . property $ do
        toks <- forAll genTokens
        tripping toks
            (pack . foldMap (renderLexeme . fmap unpack))
            testLexer

genTokens :: Gen [Tok Text]
genTokens = Gen.list (Range.linear 0 100) genToken

-- HLine 0 - prob should use 'Natural'
genToken :: Gen (Tok Text)
genToken =
    Gen.choice
        [ pure Cross
        , pure VLine
        ,      Label        <$> name --TODO numeric label
        ,      HLine        <$> smallNumber <*> pure 0
        , pure REdge
        , pure FEdge
        ,      Number       <$> number
        ,      Contact      <$> name
        ,      Coil         <$> name
        ,      Continuation <$> name
        , pure Return
        ,      Jump'        <$> name --TODO numeric label
        ,      Name         <$> name
        ,      Comment      <$> name --TODO richer alphabet
        ,      Pragma       <$> name --TODO richer alphabet
        ]
    where
    name = Gen.text (Range.linear 1 20) Gen.alpha
    smallNumber = Gen.int (Range.linear 0 999999)
    number = Gen.int (Range.linear 0 maxBound)

simpleResult :: (Bifunctor f, IsList e) => f e a -> f Bool a
simpleResult = bimap ((>0).length.toList) id

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
        tip (focus (Zp [2,1] [])) @=? Just 2
    ,  testCase "nothing to focus to" $
        tip (focus (zpFromList [])) @=? (Nothing @())
    ]

dgpTests = testGroup "Diagram parser"
    [ testCase "length" $
        dgLength (mkDgZp []) @=? 0
    , testCase "trivial" $
        fst <$> applyDgp (pure ()) (mkDgZp []) @=? Right ()
    , testCase "dgIsEmpty positive case" $
        simpleResult (fst <$> applyDgp dgIsEmpty (mkDgZp [])) @=? Right ()
    , testCase "dgIsEmpty negative case" $
        simpleResult (fst <$> applyDgp dgIsEmpty someDg) @=? Left True
    , testCase "trim 1" $
        dgTrim (Zp [] [(1, Zp [] [])]) @=? mkDgZp @(Tok Text) []
    , testCase "trim 2" $
        dgTrim someDg
            @=? mkDgZp @(Tok Text) [(1, [((1, 1), VLine)])]
    ]
    where
    someDg = Zp [] [(1, Zp [] [((1, 1), VLine)])]


ladderTests = testGroup "Ladder parser"
    [ testCase "test00" $
        (fmap (const ()) . fst) <$> dgParse t00
--             @?= Right ( Pos (-1,-1) :< LadderParser.End )
            @?= Right (() :< Source (() :< End))
    , testCase "test00" $ fullyConsumed t00
    , testCase "test01" $ fullyConsumed' test01
    , testCase "test04" $ fullyConsumed t04
--     , testCase "test05" $
--         fmap (dgTrim.psStr.snd) (applyDgp test002 (mkDgZp t05))
--             @?= Right (Zp [] [])
    , testCase "test07a" $ fullyConsumed t07a
--     , testCase "test07" $
--         fmap (dgTrim.psStr.snd) (applyDgp test002 (mkDgZp t07))
--             @?= Right (Zp [] [])
    , testCase "unexpected"
        $ bimap null id (fmap getDg
            $ dgParse [ (1, [((1, 1), Return)]) ])
        @?= Left False
    , testCase "gap"
        $ simpleResult (fmap getDg $ dgParse
            [ (1, [((1, 1), VLine)])
            , (2, [((1, 1), Cross), ((2, 2), HLine 2 0), ((4, 4), HLine 2 0)])
            ])
        @?= Left True
    , testCase "testN01"
        $ simpleResult (parse testN01)
        @?= Left True
    , testCase "test10"
        $ simpleResult (() <$ parse test10)
        @?= Right ()
    ]
    where

    dgParse = applyDgp test002' . mkDgZp
    getDg = dgTrim.psStr.snd
    fullyConsumed tk = getDg <$> dgParse tk @?= Right (Zp [] [])

--     fullyConsumed' tk
--         = getDg <$> (either (id) (dgParse) (preproc4'' tk))
--         @?= Right (Zp [] [])

    fullyConsumed' tk = parse tk @?= Right (Zp [] [])

    parse = preproc5'' >=> dgParse >=> return.getDg
    
    Right t00 = test00_tokenized
--     Right t01 = test01_tokenized
    Right t04 = test04_tokenized
    Right t05 = test05_tokenized
    Right t07 = test07_tokenized
    Right t07a = test07a_tokenized

test00_tokenized = preproc4'' test00

-- test01_tokenized = preproc4'' test01
test04_tokenized = preproc4'' test04
test05_tokenized = preproc4'' test05
test07_tokenized = preproc4'' test07
test07a_tokenized = preproc4'' test07a

test00 =
    [text|
    (* --- test 00 --- *)

    |                          |]

test01 =
    [text|
    (* --- test 01 --- *)
    
    { variables:%IX0,%QX0
      scan:100ms
    }
    
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

test05 =
    [text|
    (* --- test 05 --- *)

    | %MX0
    +--[ ]-->>MAIN
    | %QX0 %MX0
    +--(S)--(S)--
    |
    MAIN:
    | %IX0 %QX0
    +--[ ]--( )--
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
    (* --- test 07a --- *)

    |    %QX0
    +--+--( )--
    |                          |]

test10 =
    [text|
    (* --- test 10 --- *)

    | %IW0 %QX0
    +--[>]--( )--
    |  300                     |]

testN01 =
    [text|
    (* --- test N01 --- *)

    |     a
    +-- --( )--
    |                          |]

{-

    | %IW0 %QX0
    +--[>]--(S)--
    |  30
    | %IW0 %QX0
    +--[<]--(R)--
    |  20

-}


--------------------------------------------------------------------------------

testBox ln input
    = bimap (Data.Text.unpack) mkDgZp (preproc4'' input)
    >>= applyDgp (box001 ln)
-- fmap (dgTrim.psStr.snd) 

boxTests = testGroup "Box parser"
    [ testCase "1" $
        fmap (dgTrim.psStr.snd) (applyDgp (box001 2) (mkDgZp box01_tokenized))
            @?= Right (Zp [] [])
--     , testCase "1b" $
--         box01b_tokenized
--             @?= []
--     , testCase "1b" $
--         fmap (dgTrim.psStr.snd) (applyDgp (box001 2) (mkDgZp box01b_tokenized))
--             @?= Right (Zp [] [])
    , testCase "2" $
        fmap (dgTrim.psStr.snd) (testBox 2 box02)
            @?= Right (Zp [] [])
    , testCase "2a" $
        fmap (dgTrim.psStr.snd) (testBox 3 box02)
            @?= Right (Zp [] [])
    , testCase "2b" $
        fmap (dgTrim.psStr.snd) (testBox 4 box02)
            @?= Right (Zp [] [])
    ]
    where
    Right box01_tokenized = preproc4'' box01
--     Right box01b_tokenized = preproc4'' box01b

box01 =
    [text|
    +-+
    | |
    +-+                        |]

-- box01b =
--     [text|
--      +-+
--     0| |
--      +-+                        |]

box02 =
    [text|
    +-----+
    |     |
    >     |
    <     |
    +-----+                    |]

box03 =
    [text|
    |   +-----+
    |   |     |
    +--0|     |
    |   |     |
        +-----+                 |]

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
