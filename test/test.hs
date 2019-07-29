{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeApplications #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Range
import System.Environment

-- import Data.List
-- import Data.Ord
import Control.Monad
import NeatInterpolation
import Data.Text (Text, pack, unpack)
-- import qualified Data.Text
import Data.Bifunctor
import Data.Function
import Data.Traversable
import Data.Foldable hiding (toList)

import System.Directory
import System.FilePath
import qualified Data.Text.IO as TIO
-- import qualified Data.Text as T

import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Simple
import Language.Ladder.Interpreter

-- import Debug.Trace

import TestUtils

--------------------------------------------------------------------------------

--discards whitespace tokens
preproc5'
    :: Text
    -> Either Text [(Int, [((Int, Int), Tok Text)])]
preproc5' = preproc . runLexer
    where
    preproc = fmap (stripPos . dropWhitespace)

--------------------------------------------------------------------------------

preproc5'' :: Text -> Either String [(Int, [((Int, Int), Tok Text)])]
preproc5'' = bimap unpack id . preproc5'

--basic blocks
testPreproc6 :: Text -> Either Text [(Maybe Text, [[Tok Text]])]
testPreproc6
    = fmap (fmap (fmap (fmap (snd . fmap (fmap snd))))) . fmap labeledRungs . preproc5'

testPreproc4 :: Text -> Either Text [[Tok Text]]
testPreproc4 = fmap (fmap (snd . fmap (fmap snd))) . preproc5'

testPreproc5 :: Text -> Either Text [Tok Text]
testPreproc5 = fmap concat . testPreproc4

--keep comments and pragmas, drop position info, concat lines
testLexer :: Text -> Either Text [Tok Text]
testLexer = fmap dropPos . runLexer

simpleResult :: (Bifunctor f, Eq e, Monoid e) => f e a -> f Bool a
simpleResult = bimap (/=mempty) id

isEmpty :: (Eq a, Monoid a) => a -> Bool
isEmpty = (==mempty)

checkSyntax :: Text -> Either String ()
checkSyntax s
    = () <$
    (preproc5'' s >>= runLadderParser_ (wrapDevice3 (pure.fromIntegral) (pure.fromIntegral)) ladder)

assertFullyConsumed :: [(Int, [((Int, Int), Tok Text)])] -> Assertion
assertFullyConsumed tk
    = (() <$ runLadderParser (wrapDevice3 (pure.fromIntegral) (pure.fromIntegral)) ladder tk)
    @?= Right ()

--------------------------------------------------------------------------------

tokenizerTests :: TestTree
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
            $ testPreproc6 $ [text|
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
    withTests 200 . property $ do
        tok <- forAll genToken
        tripping [tok]
            (pack . foldMap (renderLexeme . fmap unpack))
            testLexer

#if 0
prop_trip :: Property
prop_trip =
    withTests 1000 . property $ do
        toks <- forAll genTokens
        tripping toks
            (pack . foldMap (renderLexeme . fmap unpack))
            testLexer

genTokens :: Gen [Tok Text]
genTokens = Gen.list (Range.linear 0 100) genToken
#endif

-- HLine 0 - prob should use 'Natural'
genToken :: Gen (Tok Text)
genToken =
    Gen.choice
        [ pure Cross
        , pure VLine
        ,      Label        <$> name --TODO numeric label
        ,      HLine        <$> smallNonZero <*> pure 0
        , pure REdge
        , pure FEdge
        ,      Number       <$> number
        ,      Contact      <$> name
        ,      Coil         <$> name
        ,      Continuation <$> name
        , pure Return
        ,      Jump'        <$> name --TODO numeric label
        ,      Name         <$> name
        ,      Comment      <$> pure <$> name --TODO richer alphabet
        ,      Pragma       <$> pure <$> name --TODO richer alphabet
        , pure NewLine
        ,      Whitespace   <$> smallNonZero
        ]
    where
    name = Gen.text (Range.linear 1 20) Gen.alpha
--     smallNumber = Gen.int (Range.linear 0 999999)
    smallNonZero = Gen.int (Range.linear 1 999)
    number = Gen.int (Range.linear 0 maxBound)

--------------------------------------------------------------------------------

zipperTests :: TestTree
zipperTests = testGroup "Zipper"
    [ testCase "from/to list" $
        zpToList <$> (stepRight $ zpFromList [1::Int,2,3,4])
            @=? Just [1,2,3,4]
    , testCase "length" $
        zpLength <$> (stepRight $ zpFromList [0::Int,1,2,3])
            @=? Just 4
    , testCase "bad move left" $
        stepLeft (zpFromList [1::Int,2])
            @=? Nothing
    , testCase "bad move right" $
        (stepRight >=> stepRight) (zpFromList [1::Int])
            @=? Nothing
    , testCase "okay move" $
        (stepRight (zpFromList [1::Int,2]) >>= tip)
            @=? Just 2
    , testCase "move there and back" $
        (stepRight (zpFromList [1::Int,2]) >>= stepLeft >>= tip)
            @=? Just 1
    , testCase "okay focus" $
        tip (focus (Zp [2::Int,1] []))
            @=? Just 2
    , testCase "nothing to focus to" $
        tip (focus (zpFromList []))
            @=? Nothing @()
    , testCase "zpLookup, found" $
        zpLookup 2 (zpFromList [(1::Int, ()), (2, ()), (3, ())])
            @=? Zp [(1, ())] [(2, ()), (3, ())]
    , testCase "zpLookup, not found" $
        zpLookup 4 (zpFromList [(1::Int, ()), (2, ()), (3, ())])
            @=? Zp [(3, ()), (2, ()), (1, ())] []
    , testCase "fmap + show" $
        show (fmap not $ zpFromList [True])
            @=? "Zp [] [False]"
    ]

--------------------------------------------------------------------------------

dgpTests :: TestTree
dgpTests = testGroup "Diagram parser"
    [ testCase "length" $
        dgLength (mkDgZp []) @=? 0
    , testCase "trivial" $
        fst <$> applyDgp (pure ()) (mkDgZp []) () @=? Right ()
    , testCase "dgIsEmpty positive case" $
        simpleResult (fst <$> applyDgp dgIsEmpty (mkDgZp []) ()) @=? Right ()
    , testCase "dgIsEmpty negative case" $
        simpleResult (fst <$> applyDgp dgIsEmpty someDg ()) @=? Left True
    , testCase "trim 1" $
        dgTrim (Zp [] [(1, Zp [] [])]) @=? mkDgZp @(Tok Text) []
    , testCase "trim 2" $
        dgTrim someDg
            @=? mkDgZp @(Tok Text) [(1, [((1, 1), VLine)])]
    ]
    where
    someDg = Zp [] [(1, Zp [] [((1, 1), VLine)])]

--------------------------------------------------------------------------------

ladderTests :: TestTree
ladderTests = testGroup "Ladder parser"
    [ testCase "test00" $
        (fmap (const ())) <$> runLadderParser_ parseSimpleDevice ladder t00
            @?= Right (() :< Source (() :< End))
    , testCase "test00" $ assertFullyConsumed t00
    , testCase "test01" $ fullyConsumed' test01
    , testCase "test04" $ assertFullyConsumed t04
    , testCase "test07a" $ assertFullyConsumed t07a
    , testCase "unexpected"
        $ simpleResult (runLadderParser_ parseSimpleDevice ladder [ (1, [((1, 1), Return)]) ])
            @?= Left True
    , testCase "gap"
        $ simpleResult (runLadderParser_ parseSimpleDevice ladder
            [ (1, [((1, 1), VLine)])
            , (2, [((1, 1), Cross), ((2, 2), HLine 2 0), ((4, 4), HLine 2 0)])
            ])
        @?= Left True
    , testCase "testN01"
        $ simpleResult (checkSyntax testN01)
            @?= Left True
    , testCase "test10"
        $ simpleResult (checkSyntax test10)
            @?= Right ()
    ]
    where

    fullyConsumed' tk = checkSyntax tk @?= Right ()

    Right t00 = test00_tokenized
    Right t04 = test04_tokenized
--     Right t05 = test05_tokenized
--     Right t07 = test07_tokenized
    Right t07a = test07a_tokenized

    test00_tokenized = preproc5' test00

    test04_tokenized = preproc5' test04
--     test05_tokenized = preproc5' test05
--     test07_tokenized = preproc5' test07
    test07a_tokenized = preproc5' test07a

test00, test01, test04, test07a, test10, testN01 :: Text

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

-- test05 =
--     [text|
--     (* --- test 05 --- *)
-- 
--     | %MX0
--     +--[ ]-->>MAIN
--     | %QX0 %MX0
--     +--(S)--(S)--
--     |
--     MAIN:
--     | %IX0 %QX0
--     +--[ ]--( )--
--     |                          |]

-- test07 =
--     [text|
--     (* --- test 07 --- *)
-- 
--     | %IX0    %QX0
--     +--[ ]--+--( )--
--     | %IX1  |
--     +--[ ]--+
--     |                          |]

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

analysisTests :: TestTree
analysisTests = testGroup "Analysis"
    [ testCase "sttsort 1" $ do
--         print g01'
        isSpatialOrTopo gDepends (compare `on` snd) g01'
            @?= Nothing
    , testCase "sttsort 2"
        $ ts [([2], 1)] @?= [1]
    , testCase "sttsort 3" $ ts
            [ ([],  1)
            , ([1], 2)
            , ([],  3)
            , ([1], 4)
            ]
            @?= [1,2,3,4]
    , testCase "sttsort 4" $ ts
            [ ([],  1)
            , ([3], 2)
            , ([] , 3)
            , ([1], 4)
            ]
            @?= [1,3,2,4]
    , testCase "sttsort 5" $ ts
            [ ([2], 1)
            , ([1], 2)
            , ([], 3)
            ]
            @?= [2,1,3]
    , testCase "sttsort 6" $ ts
            [ ([2,3], 1)
            , ([], 2)
            , ([2], 3)
            , ([], 4)
            ]
            @?= [2,3,1,4]
    , testProperty "sttsort isSpatialOrTopo" prop_sttsort
    ]

    where
    ts = fmap snd . sttsort gDepends

--     let dep01 = (\(as, a) (bs, b) -> elem b as)
--     let ts = sttsort dep01
--     let testts lbl s =
--             let s2 = ts s
--             in print
--                 ( lbl
--                 , istopo dep01 s
--                 , istopo dep01 s2
--                 , istopoM dep01 s2
--                 , fmap
--                     (\p -> iscycle (on (==) snd) dep01 p s2)
--                     $ istopoM dep01 s2
--                 , s2)
g01, g01' :: [([Int], Int)]
g01' = sttsort gDepends g01
g01 =
    [ ( [ 7 , 4 ] , 2 )
    , ( [] , 3 )
    , ( [] , 4 )
    , ( [ 3 ] , 7 )
    ]

genGraph :: Gen [([Int], Int)]
genGraph = do --Gen.sample $ 
    let k = 10
    n <- Gen.int $ constant 0 k
--     let l = [0..n]
    ll <- for [0..n] $ \v -> do
        deps <- Gen.list (linear 0 k) (Gen.int $ constant 0 k)
        return (deps, v)
    return ll

gDepends :: ([Int], Int) -> ([Int], Int) -> Bool
gDepends = (\(as, _a) (_bs, b) -> elem b as)

prop_sttsort :: Property
prop_sttsort =
    withTests 1000 . property $ do
        g <- forAll genGraph
        let g' = sttsort gDepends g
        Nothing === isSpatialOrTopo gDepends (compare `on` snd) g'

--------------------------------------------------------------------------------

testBox :: Int -> Text -> Either String (Dg (Tok Text))
testBox ln input
    = preproc5'' input >>= runLadderParser parseSimpleDevice (box001 ln) >>= (return . snd)

boxTests :: TestTree
boxTests = testGroup "Box parser"
    [ testCase "1" $
        fmap (dgTrim.snd) (runLadderParser parseSimpleDevice (box001 2) box01_tokenized)
            @?= Right (Zp [] [])
--     , testCase "1b" $
--         box01b_tokenized
--             @?= []
--     , testCase "1b" $
--         fmap (dgTrim.psStr.snd) (applyDgp (box001 2) (mkDgZp box01b_tokenized))
--             @?= Right (Zp [] [])
    , testCase "2" $
        fmap dgTrim (testBox 2 box02)
            @?= Right (Zp [] [])
    , testCase "2a" $
        fmap dgTrim (testBox 3 box02)
            @?= Right (Zp [] [])
    , testCase "2b" $
        fmap dgTrim (testBox 4 box02)
            @?= Right (Zp [] [])
    ]
    where
    Right box01_tokenized = preproc5' box01
--     Right box03_tokenized = preproc5' box03
--     Right box01b_tokenized = preproc5' box01b

box01, box02 :: Text

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

#if 0
box03 =
    [text|
    |   +-----+
    |   |     |
    +--0|     |
    |   |     |
        +-----+                 |]
#endif

--------------------------------------------------------------------------------

otherTests :: TestTree
otherTests = testGroup "Other tests"
    [ testCase "show ast" $
        show (() :< Nothing)
            @=? "(() :< Nothing)"
    , testCase "fold ast" $
        fold (() :< Nothing)
            @=? ()
    , testCase "get signal 0" $
        getSignal "x" vect trace1
            @?= [[I 2],[I 5]]
    , testCase "get signal 1" $
        getSignal "y" vect trace1
            @?= [[I 3], [I 5]]
    , testCase "get signal 1" $
        getSignals ["x", "y"] vect trace1
            @?= [[I 3, I 3], [I 5, I 3]]
    ]
    where
    vect = [ (1, [("v", I 0), ("x", X False), ("y", X False)])
               , (1, [("v", I 5)])
               , (1, [("v", I 10)])
               , (1, [("v", I 20)])
               ]
--   , watch    = ["x", "y"]
    trace1 =
        [ [I 1,I 2,I 3]
        , [I 4,I 5,I 5]
        ]
--     xxx = [[X False, X True], [X False, X True], [X False, X False], [X True, X False]]
--     expected0 = fmap ((:[]) . (!! 0)) xxx
--     expected1 = fmap ((:[]) . (!! 1)) xxx

--------------------------------------------------------------------------------

basicTests :: [TestTree]
basicTests = 
    [ tokenizerTests
    , zipperTests
    , dgpTests
    , ladderTests
    , boxTests
    , analysisTests
    , otherTests
    ]

getTests :: IO TestTree
getTests = do
    ftPos <- fileTests "test"
    ftNeg <- fileTestsNeg $ "test" </> "negative"
    return $ testGroup "Tests" [testGroup "Basic" basicTests, ftPos, ftNeg]

--------------------------------------------------------------------------------

testFromDirectory :: FilePath -> TestName -> (FilePath -> IO TestTree) -> IO TestTree
testFromDirectory path name mkCase = do
    files <- filter ((".txt"==).takeExtension) <$> listDirectory path
--     print (here, files)
    tests <- for files $ \fn -> mkCase fn
    return $ testGroup name tests

fileTestsNeg :: FilePath -> IO TestTree
fileTestsNeg path
    = testFromDirectory path "File tests - negative" $ \fn -> do
        src <- TIO.readFile $ path </> fn
        return $ testCase fn $ do
            case preproc5' src of
                 Left _ -> return () --preproc failed -> test succeeeded
                 Right lxs ->
                    case snd <$> runLadderParser parseSimpleDevice ladder lxs of
                        Right (Zp [] []) -> assertFailure here
                        Left _ -> return ()
                        _ -> return ()


fileTests :: FilePath -> IO TestTree
fileTests path
    = testFromDirectory path "File tests - positive" $ \fn -> do
        return $ testCase fn $ do
            (tst, lxs) <- fmap dropWhitespace <$> loadLadderTest (path </> fn)
            let blocks = labeledRungs lxs
            case tst of
                Nothing -> do
                    for_ blocks $ \(_, lxs') -> do
                        case runLadderParser parseSimpleDevice ladder lxs' of
                            Right _ -> return ()
                            Left err -> fail err
                Just t -> do
                    ast <- parseOrDie2
                            (wrapDevice3
                                    (pure . I)
                                    (pure . A) -- _ :: String -> Either String Int
                                    --litFromAddr
                                    )
                            lxs --parseSimpleDevice
                    passed <- runLadderTest2 False t ast
                    passed @?= True

--------------------------------------------------------------------------------

main :: IO ()
main = do
    setEnv "TASTY_NUM_THREADS" "4"
    getTests >>= defaultMain
