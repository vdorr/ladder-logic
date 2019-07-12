#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module TestUtils where

import Data.List
import Text.Read
import qualified Data.Text.IO as TIO
import Data.Text (Text, unpack)
-- import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.Traversable
import Data.Semigroup
import Data.Void

-- import Control.Monad.Except

-- import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Interpreter
import Language.Ladder.Simple

import Tooling

--------------------------------------------------------------------------------

data LadderTest = T01
    { testVect :: [(Int, [(String, V)])]
    , watch :: [String]
    , expected :: [[V]]
    } deriving (Show, Read)

--------------------------------------------------------------------------------

getSignal :: String -> TestVect -> [[V]] -> [[V]]
getSignal _ []               = const []
getSignal s ((_, slice) : _) = fmap ((:[]).(!!i))
    where
    Just i = findIndex ((s==) . fst) slice

getSignals :: [String] -> TestVect -> [[V]] -> [[V]]
getSignals sg vect trace = 
    foldMap (\s -> getSignal s vect trace) sg

runLadderTest
    :: Bool
    -> LadderTest
    -> [(Maybe String, Cofree (Diagram Void (Dev String) String) DgExt)]
    -> IO Bool
runLadderTest verbose test@T01{} ast = do
    when verbose $ print here

--     prog <- generateStk1 ast
    blocks <- for ast (traverse generateStk2)

    let allSigs = testVectSignals (testVect test)
--TODO select signals for display independently from signals for test evaluation
--     let displaySigs = allSigs -- or (watch test)
    when verbose $ print (here, allSigs)

    let xxy = evalTestVect''' blocks allSigs (testVect test)

    when verbose $ print (here, xxy)
    let Right traces = xxy
    when verbose $ putStrLn $ unlines $ prettyTrace $ zip allSigs $ transpose traces

--     let idxs = fmap (findIndex)
    let testTrace = getSignals (watch test) (testVect test) traces
    when verbose $ print (here, testTrace)
    when verbose $ print (here, expected test)

    let passed = expected test == testTrace
--     let passed = False
    when verbose $ print (here, passed, if passed then "PASSED" else "FAILED")

    return passed

--------------------------------------------------------------------------------

ldUnpack :: Cofree (Diagram c (Dev Text) Text) a
         -> Cofree (Diagram c (Dev String) String) a
ldUnpack (a :< n) = a :< fmap ldUnpack (mapDg id (fmap unpack) unpack n)

-- |also return pragmas
parseOrDie5
    :: FilePath
    -> IO ( [String]
          , [(Maybe String, Cofree (Diagram (Void) (Op String (Operand String)) String) DgExt)]
          )
parseOrDie5 path = do
    (_, lxs)    <- loadLadderTest path
    ast         <- parseOrDie2 $ dropWhitespace lxs
    let pragmas  = fmap unpack $ getLeadingPragmas $ dropPos lxs
    ast'        <- traverse (traverse (either fail return . parseOpsM)) ast
    return (pragmas, ast')

-- |also return pragmas
parseOrDie4
    :: FilePath
    -> IO ( [String]
          , [(Maybe String, Cofree (Diagram Void (Dev String) String) DgExt)]
          )
parseOrDie4 path = do
    (_, lxs)    <- loadLadderTest path
    ast         <- parseOrDie2 $ dropWhitespace lxs
    let pragmas  = fmap unpack $ getLeadingPragmas $ dropPos lxs
    return (pragmas, ast)

parseOrDie3
    :: FilePath
    -> IO [(Maybe String, Cofree (Diagram Void (Dev String) String) DgExt)]
parseOrDie3 path = do
    (_tst, lxs) <- fmap dropWhitespace <$> loadLadderTest path
    parseOrDie2 lxs

-- |like 'parseOrDie' but additionaly can handle labels
parseOrDie2
    :: [(Int, [((Int, Int), Tok Text)])]
    -> IO [(Maybe String, Cofree (Diagram Void (Dev String) String) DgExt)]
parseOrDie2 lxs = do
    let blocks = labeledRungs lxs
    for blocks (\(lbl, p) -> (fmap unpack lbl,) <$> parseOrDie p)

-- |assuming comments and pragmas were filtered out
parseOrDie
    :: [(Int, [((Int, Int), Tok Text)])]
    -> IO (Cofree (Diagram Void (Dev String) String) DgExt)
parseOrDie lxs = do
--    let zp = mkDgZp $ dropWhitespace lxs
#if 0
    forM_ (zpToList zp) (print . (here,))
#endif
--    case applyDgp parseLadder zp () of
    case runLadderParser_ ladder lxs of
--         Right (ast, (DgPSt _ c@(Zp zpl zpr) _ _ _)) -> do
        Right ast -> return $ ldUnpack ast
        Left err -> fail $ show (here, err)


loadLadderTest :: FilePath -> IO (Maybe LadderTest, [(Int, [((Int, Int), Tok Text)])])
loadLadderTest file = do
    src <- TIO.readFile file
    case stripPos <$> runLexer src of
        Left err -> fail $ show (here, err)
        Right x -> do
--             print (here, getPragma $ tokens x)
--             let Just pgma = fmap (filter (/='\\') . T.unpack) $getPragma $ tokens x
            let pgma = fmap (filter (/='\\') . unpack) $ getPragma $ dropPos x
--             print ((read pgma) :: LadderTest)
            return (pgma >>= readMaybe, x)
--                     fail $ show (here, "no embedded test found")

--------------------------------------------------------------------------------

istopo :: (a -> a -> Bool) -> [a] -> Bool
istopo dep (x : xs) = all (\y -> not $ dep x y) xs && istopo dep xs
istopo _   []       = True

istopoM :: (a -> a -> Bool) -> [a] -> Maybe a
istopoM dep (x : xs) = fst (pickFirst (dep x) xs) <|> istopoM dep xs
istopoM _   []       = Nothing

-- isSpatialOrTopo :: (a -> a -> Bool) -> (a -> a -> Ordering) -> [a] -> Maybe a
-- isSpatialOrTopo dep spa g = go g
--     where
--     go (x : xs : xss)
--         | spa x xs == LT || any (flip dep x) (xs:xss) = go (xs : xss)
--         | otherwise = Just x
--     go _ = Nothing
isSpatialOrTopo :: (a -> a -> Bool) -> (a -> a -> Ordering) -> [a] -> Maybe (a, a)
isSpatialOrTopo dep spa g = (,) <$> istopoM dep g <*> isSorted sources
    where
    isSorted (x:xs:xss)
        | spa x xs == LT = isSorted (xs:xss)
        | otherwise      = Just x
    isSorted _          = Nothing

--TODO i should check if this fires in hedgehog
--is it true that this list is spatially sorted?
    sources = filter noPreds g
    noPreds v = all (\w -> spa v w /= EQ && not (dep v w)) g


iscycle :: (a -> a -> Bool) -> (a -> a -> Bool) -> a -> [a] -> Bool
iscycle eq dep x = go x
    where
    go a as = case depend of
                   [] -> False
                   d | any (dep x) depend -> True --flip dep?
                   _ -> any (flip go indep) depend
        where
        (depend, indep) = partition (flip dep a) as

sameLine :: Cofree (Diagram c d s) DgExt -> Bool
sameLine n@((ln, _) :< _) = getAll $ foldMap (All.(ln==).fst) n
