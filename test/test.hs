{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeApplications, OverloadedLists #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
-- import Hedgehog.Range
import System.Environment

-- import Data.List
import Data.Void
import Control.Monad
import NeatInterpolation
import Data.Text (Text, pack, unpack)
-- import qualified Data.Text
import Data.Bifunctor
-- import Data.Function
import Data.Traversable
import Data.Foldable -- hiding (toList)
import GHC.Exts hiding (toList)

import System.Directory
import System.FilePath
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Simple
import Language.Ladder.Interpreter
import Language.Ladder.Types
import Language.Ladder.Box
import Language.Ladder.Tooling
import Language.Ladder.Eval

import TestUtils

--------------------------------------------------------------------------------

--discards whitespace tokens
preproc5 :: Text -> Either String [[(DgExt, Tok Text)]]
preproc5 = fmap dropWhitespace . runLexer

--------------------------------------------------------------------------------

--basic blocks
testPreproc6 :: Text -> Either String [(Maybe (Either Int Text), [[Tok Text]])]
testPreproc6
    = fmap ((fmap (fmap (fmap (fmap snd)))) . labeledRungs) . preproc5

testPreproc4 :: Text -> Either String [[Tok Text]]
testPreproc4 = fmap dropPos2 . preproc5

testPreproc5 :: Text -> Either String [Tok Text]
testPreproc5 = fmap concat . testPreproc4

--keep comments and pragmas, drop position info, concat lines
testLexer :: Text -> Either String [Tok Text]
testLexer = fmap dropPos . runLexer

simpleResult :: (Bifunctor f, Eq e, Monoid e) => f e a -> f Bool a
simpleResult = first (/=mempty)

-- isEmpty :: (Eq a, Monoid a) => a -> Bool
-- isEmpty = (==mempty)

checkSyntax :: Text -> Either String ()
checkSyntax s
    = () <$
    (preproc5 s >>=
        runLadderParser_ wrapDeviceForTest ladder)

assertFullyConsumed :: [[(DgExt, Tok Text)]] -> Assertion
assertFullyConsumed tk
    = (() <$ runLadderParser wrapDeviceForTest ladder tk)
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
        (Right [[VLine],[Cross,HLine 0 0,Jump' (Right "LBL")]] @=?) $ testPreproc4 $ [text|
            | (* hello *)
            +->>LBL         |]
    , testCase "label" $
        (Right [[VLine],[Label (Right "LBL")],[VLine]] @=?) $ testPreproc4 $ [text|
            | (* hello *)
            LBL:
            |               |]
    , testCase "label/blocks" $
        (Right [(Nothing, [[VLine]]),(Just (Right "LBL"), [[VLine]])] @=?)
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
        ,      Label        <$> Right <$> shortName --TODO numeric label
        ,      HLine        <$> smallNonZero <*> pure 0
        , pure REdge
        , pure FEdge
        ,      Number       <$> biggishNumber
        ,      Contact      <$> shortName
        ,      Coil         <$> shortName
        ,      Continuation <$> shortName
        , pure Return
        ,      Jump'        <$> Right <$> shortName --TODO numeric label
        ,      Name         <$> shortName
        ,      Comment      <$> pure <$> shortName --TODO richer alphabet
        ,      Pragma       <$> pure <$> shortName --TODO richer alphabet
        , pure NewLine
        ,      Whitespace   <$> smallNonZero
        ]
    where
    shortName = Gen.text (Range.linear 1 20) Gen.alpha
--     smallNumber = Gen.int (Range.linear 0 999999)
    smallNonZero = Gen.int (Range.linear 1 999)
    biggishNumber = Gen.int (Range.linear 0 maxBound)

--------------------------------------------------------------------------------

zipperTests :: TestTree
zipperTests = testGroup "Zipper"
    [ testCase "from/to list" $
        toList <$> (stepRight $ fromList [1::Int,2,3,4])
            @=? Just [1,2,3,4]
    , testCase "length" $
        length <$> (stepRight $ fromList [0::Int,1,2,3])
            @=? Just 4
    , testCase "bad move left" $
        stepLeft (fromList [1::Int,2])
            @=? Nothing
    , testCase "bad move right" $
        (stepRight >=> stepRight) (fromList [1::Int])
            @=? Nothing
    , testCase "okay move" $
        (stepRight (fromList [1::Int,2]) >>= tip)
            @=? Just 2
    , testCase "move there and back" $
        (stepRight (fromList [1::Int,2]) >>= stepLeft >>= tip)
            @=? Just 1
    , testCase "okay focus" $
        tip (focus (Zp [2::Int,1] []))
            @=? Just 2
    , testCase "nothing to focus to" $
        tip (focus (fromList []))
            @=? Nothing @()
    , testCase "zpLookup, found" $
        zpLookup 2 (fromList [(1::Int, ()), (2, ()), (3, ())])
            @=? Zp [(1, ())] [(2, ()), (3, ())]
    , testCase "zpLookup, not found" $
        zpLookup 4 (fromList [(1::Int, ()), (2, ()), (3, ())])
            @=? Zp [(3, ()), (2, ()), (1, ())] []
    , testCase "fmap + show" $
        show (fmap not ([True] :: Zp Bool))
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
        dgTrim (Zp [] [(Zp [] [])]) @=? mkDgZp @(Tok Text) []
    , testCase "trim 2" $
        dgTrim someDg
            @=? mkDgZp @(Tok Text) [[((1, (1, 1)), VLine)]]
    ]
    where
    someDg = Zp [] [Zp [] [((1, (1, 1)), VLine)]]

--------------------------------------------------------------------------------

ladderTests :: TestTree
ladderTests = testGroup "Ladder parser"
    [ testCase "test00" $
        (fmap (const ())) <$> runLadderParser_ wrapDevTest1 ladder t00
            @?= Right (() :< Source (() :< End))
    , testCase "test00" $ assertFullyConsumed t00
    , testCase "test01" $ fullyConsumed' test01
    , testCase "test04" $ assertFullyConsumed t04
    , testCase "test07a" $ assertFullyConsumed t07a
    , testCase "unexpected"
        $ simpleResult (runLadderParser_ wrapDevTest1 ladder [ [((1, (1, 1)), Return)] ])
            @?= Left True
    , testCase "gap"
        $ simpleResult (runLadderParser_ wrapDevTest1 ladder
            [ [((1, (1, 1)), VLine)]
            , [((2, (1, 1)), Cross), ((2, (2, 2)), HLine 2 0), ((2, (4, 4)), HLine 2 0)]
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

    test00_tokenized = preproc5 test00

    test04_tokenized = preproc5 test04
--     test05_tokenized = preproc5 test05
--     test07_tokenized = preproc5 test07
    test07a_tokenized = preproc5 test07a

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
    [ 
    ]

--------------------------------------------------------------------------------

testBox :: Int -> Text -> Either String (Dg (Tok Text))
testBox ln input
    = preproc5 input >>= runLadderParser wrapDeviceSimple (box001 ln) >>= (return . snd)

boxTests :: TestTree
boxTests = testGroup "Box parser"
    [ testCase "1" $
        fmap (dgTrim.snd) (runLadderParser wrapDeviceSimple (box001 2) box01_tokenized)
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
    Right box01_tokenized = preproc5 box01
--     Right box03_tokenized = preproc5 box03
--     Right box01b_tokenized = preproc5 box01b

box01, box02 :: Text

box01 = T.unlines
        [ "+-+"
        , "| |"
        , "+-+"
        ]

--     [text|
--     +-+
--     | |
--     +-+                        |]

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
--     , testCase "get signal 0" $
--         getSignal "x" vect trace1
--             @?= [[I 2],[I 5]]
--     , testCase "get signal 1" $
--         getSignal "y" vect trace1
--             @?= [[I 3], [I 5]]
    , testCase "get signals" $
        getSignals ["x", "y"] vect trace1
            @?= [[I 2, I 3], [I 5, I 5]]
    ]
    where
    vect :: TestVect String
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
    ftPos <- fileTests actualFileTest4 $ "test" </> "data"
    ftNeg <- fileTestsNeg fileTestsNegTest4 $ "test" </> "data" </> "negative"
    return $ testGroup "Tests" [testGroup "Basic" basicTests, ftPos, ftNeg]

--------------------------------------------------------------------------------

parseForTestOrDie
    :: [[(DgExt, Tok Text)]]
    -> IO [ ( Maybe (Either Int String)
            , Cofree
                (Diagram Void
                    ([(CellType, Operand Text)], DeviceImpl (V addr) addr) (Either Int String))
                DgExt)
            ]
parseForTestOrDie = either fail pure . parseOrDie2 wrapDeviceForTest

wrapDeviceForTest
    :: DeviceParser Text ([(CellType, Operand Text)], DeviceImpl (V addr) addr)
wrapDeviceForTest = wrapDevice3 (pure . I) (pure . A)

testFromDirectory :: FilePath -> TestName -> (FilePath -> IO TestTree) -> IO TestTree
testFromDirectory path testName mkCase = do
    files <- filter ((".txt"==).takeExtension) <$> listDirectory path
    tests <- for files $ \fn -> mkCase fn
    return $ testGroup testName tests

--------------------------------------------------------------------------------

fileTestsNeg
    :: ([[(DgExt, Tok Text)]] -> Either String ()
--      [ExtendedInstruction Int (Instruction (V String) String)]
     )
    -> FilePath
    -> IO TestTree
fileTestsNeg tst path
    = testFromDirectory path "File tests - negative" $ \fn -> do
        src <- TIO.readFile $ path </> fn
        return $ testCase fn $ do
            case preproc5 src of
                 Left err -> do
                     print (here, fn, err)
                     return () --preproc failed -> test succeeeded
                 Right lxs -> do
--                     case parseOrDie2 wrapDeviceForTest lxs of
--                         Left  _err -> return ()
--                         Right ast   -> do
--                             case compileForTest03 ast of
--                                  Left err | length err > 0 -> return ()
-- --                                  Left _err  -> assertFailure here
-- --                                  Right _ -> assertFailure here
--                                  _ -> assertFailure here
--                     let x = parseOrDie2 wrapDeviceForTest lxs
--                                 >>= compileForTest03
                    case tst lxs of
                                 Left err | length err > 0 -> return ()
--                                  Left _err  -> assertFailure here
--                                  Right _ -> assertFailure here
                                 failed -> assertFailure $ show (here, failed)

--------------------------------------------------------------------------------

fileTests
    :: ([[[String]]] -> Maybe (LadderTest String)
                      -> [[(DgExt, Tok Text)]]
                      -> IO (Maybe String))
    -> FilePath -> IO TestTree
fileTests runTest path
    = testFromDirectory path "File tests - positive" $ \fn -> do
        return $ testCase fn $ do
            (tst, lxs'') <- loadLadderTest (path </> fn)
            let lxs = dropWhitespace lxs''
            let (pgms', _) = pickPragma "LANGUAGE" $ getLeadingPragmas $ dropPos lxs''
            let pgms = (fmap (fmap (words . unpack)) pgms')

            testFailure <- runTest pgms tst lxs
            testFailure @?= Nothing

--             let wrapper = case pgms of
--                  (["LANGUAGE" : "BottomOperandContext" : _] : _) -> wrapDeviceSimple2
--                  _ -> wrapDeviceSimple
-- 
--             case tst of
--                 Nothing -> do
--                     case runLadderParser wrapper ladder' lxs of
--                         Right _ -> return ()
--                         Left err -> fail err
--                 Just t -> do
--                     ast <- parseForTestOrDie lxs
--                     passed <- runLadderTest22 False t ast
--                     passed @?= True

wrapDevTest1 :: DeviceParser Text (DevType Text, [Operand Text])
wrapDevTest1 = wrapDeviceSimple2

--------------------------------------------------------------------------------

fileTestsNegTest4
    :: [[(DgExt, Tok Text)]]
    -> Either String ()
fileTestsNegTest4 lxs'
    = void $ parseLadder4 lxs >>= (\p -> Language.Ladder.Eval.evalM p (EvMem []))
    where
    lxs = fmap (fmap (fmap (fmap unpack))) lxs'

fileTestsNegTest1
    :: [[(DgExt, Tok Text)]]
    -> Either String ()
fileTestsNegTest1 lxs
    = void $ parseOrDie2 wrapDeviceForTest lxs >>= compileForTest03


actualFileTest1
    :: [[[String]]]
    -> Maybe (LadderTest String)
                      -> [[(DgExt, Tok Text)]]
                      -> IO (Maybe String)
actualFileTest1 pgms tst lxs = do
    let wrapper
            = case pgms of
                (["LANGUAGE" : "BottomOperandContext" : _] : _) -> wrapDeviceSimple2
                _other                                          -> wrapDeviceSimple

    case tst of
        Nothing -> do
            case runLadderParser wrapper ladder' lxs of
                Right _ -> return Nothing
                Left err ->
--                             fail err
                    return $ Just err
        Just t -> do
--             ast <- parseForTestOrDie lxs
            let Right ast = parseOrDie2 wrapDeviceForTest lxs
            passed <- runLadderTest22 False t ast
            return $ if passed then Nothing else Just "embedded test failed"


actualFileTest4 :: [[[String]]] -> Maybe (LadderTest String)
                      -> [[(DgExt, Tok Text)]]
                      -> IO (Maybe String)
actualFileTest4 pgms tst lxs' = do
    let wrapper
            = case pgms of
                (["LANGUAGE" : "BottomOperandContext" : _] : _) -> wrapDeviceSimple2
                _other                                          -> wrapDeviceSimple
    let lxs = fmap (fmap (fmap (fmap unpack))) lxs'

    case tst of
        Nothing -> do
            case runLadderParser wrapper ladder' lxs of
                Right _ -> return Nothing
                Left err ->
--                             fail err
                    return $ Just err
        Just t -> do
            let Right ast = parseLadder4 lxs
            passed <- runLadderTest4 False t ast
            return $ if passed then Nothing else Just "embedded test failed"

--------------------------------------------------------------------------------

main :: IO ()
main = do
    setEnv "TASTY_NUM_THREADS" "4"
    getTests >>= defaultMain
