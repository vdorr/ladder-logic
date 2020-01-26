{-# LANGUAGE OverloadedStrings, MonadComprehensions #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module TestUtils where

import Data.List
import Text.Read
import qualified Data.Text.IO as TIO
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.Traversable
import Data.Semigroup
import Data.Void
import Control.Monad.Except
import Data.Foldable
-- import Data.Bifunctor
import Control.Monad.Writer.Strict
import Data.Functor.Identity

import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Interpreter
import Language.Ladder.Simple

import Tooling

--------------------------------------------------------------------------------

data LadderTest = T01
    { testVect :: [(Int, [(String, V String)])]
    , watch :: [String]
    , expected :: [[V String]]
    } deriving (Show, Read)

--------------------------------------------------------------------------------

--FIXME should not be that specialized
getSignal :: Eq addr => addr -> TestVect addr -> [[V addr]] -> [[V addr]]
getSignal _ []               = const []
getSignal s ((_, slice) : _) = fmap ((:[]).(!!i))
    where
    Just i = findIndex ((s==) . fst) slice

getSignal1 :: Eq addr => addr -> TestVect addr -> [[V addr]] -> [V addr]
getSignal1 _ []               = const []
getSignal1 s ((_, slice) : _) = fmap (!!i)
    where
    Just i = findIndex ((s==) . fst) slice

getSignals :: Eq addr => [addr] -> TestVect addr -> [[V addr]] -> [[V addr]]
getSignals sg vect trace = 
    transpose $ fmap (\s -> getSignal1 s vect trace) sg

--------------------------------------------------------------------------------

emitDevice03
    :: ([(CellType, Operand Text)], DeviceImpl (V String) String)
    -> [Instruction (V String) String]
emitDevice03 = snd . emitDevice03'

emitDevice03'
    :: ([(CellType, Operand Text)], DeviceImpl (V String) String)
    -> ([String], [Instruction (V String) String])
emitDevice03' (ops, impl) = case impl (fmap unAddr ops) of
                            Left err -> error $ show (here, err)
                            Right x -> x
    where
    unAddr :: (CellType, Operand Text) -> Operand String
    unAddr (_, Var a) = Var $ unpack a
    unAddr (_, Lit i) = Lit i

compileForTest03
    :: (Show lbl, Eq lbl, MonadError String m, Monad m)
    => [(Maybe lbl, Cofree (Diagram Void
            (([(CellType, Operand Text)], DeviceImpl (V String) String))
            lbl) DgExt)]
    -> m [ExtendedInstruction Int (Instruction (V String) String)]
compileForTest03 ast = do
    (++ [EIReturn]) <$> generateStk2xx pure emitDevice03 ast

--------------------------------------------------------------------------------

runLadderTest2
    :: Bool
    -> LadderTest
    -> [(Maybe String
            , Cofree (Diagram Void 
                    (([(CellType, Operand Text)], DeviceImpl (V String) String))
                    String) DgExt)]
    -> IO Bool
runLadderTest2 verbose test ast = do
    when verbose $ print here
    prog <- either fail pure $ compileForTest03 ast
    when verbose $ do
        putStrLn "---------------------------"
        for_ prog print
        putStrLn "---------------------------"
    runLadderTestX verbose test prog


memSlotsToTestVector :: Int -> [(CellType, addr)] -> TestVect addr
memSlotsToTestVector n m = (1, fmap g m) : [(n - 1, [])]
-- [(Int, [(addr, (V addr))])]
    where
    g (t, addr) = (addr, f t)
    f Bit     = X False
    f Word    = I 0
    f TwoBits = undefined --TODO


runLadderTest22
    :: Bool
    -> LadderTest
    -> [(Maybe String
            , Cofree (Diagram Void 
                    (([(CellType, Operand Text)], DeviceImpl (V String) String))
                    String) DgExt)]
    -> IO Bool
runLadderTest22 verbose test ast = do
    when verbose $ print here
    prog <- either fail pure $ compileForTest03 ast
    let memSlots :: [(CellType, Text)] = nub $ execWriter $ traverse_ (traverse_ mp2) ast
    print (here, memSlots, "<<<<<"::String)
    print (here, memSlotsToTestVector 4 memSlots, "<<<<<"::String)

    when verbose $ do
        putStrLn "---------------------------"
        for_ prog print
        putStrLn "---------------------------"
    runLadderTestX verbose test prog

    where
    mp2 :: Cofree
                        (Diagram c' ([(CellType, Operand Text)], b) s') a
                      -> WriterT [(CellType, Text)] Identity ()
    mp2 (_ :< n) = do
        void $ mapDgA pure (tell.addressesOnly.fst) pure n
        traverse_ mp2 n

    addressesOnly s = [(t, a)|((t, Var a)) <- s]


runLadderTest221
    :: Bool
    -> Int
    -> [(Maybe String
            , Cofree (Diagram Void 
                    (([(CellType, Operand Text)], DeviceImpl (V String) String))
                    String) DgExt)]
    -> IO ()
runLadderTest221 verbose num ast = do
    when verbose $ print here
    prog <- either fail pure $ compileForTest03 ast
    let memSlots' :: [(CellType, Text)] = nub $ execWriter $ traverse_ (traverse_ mp2) ast
    let memSlots = fmap (fmap unpack) memSlots'
    print (here, memSlots, "<<<<<"::String)
    let vect = memSlotsToTestVector num memSlots
    print (here, vect, "<<<<<"::String)

    when verbose $ do
        putStrLn "---------------------------"
        for_ prog print
        putStrLn "---------------------------"
--     runLadderTestX verbose test prog
    let allSigs = fmap snd memSlots
    let xxy = evalTestVect''' prog allSigs vect

--     when verbose $ print (here, xxy)
    let Right traces = xxy

    print (here, traces)
    when verbose $ putStrLn $ unlines $ prettyTrace $ zip allSigs $ transpose traces

    return ()

    where
    mp2 :: Cofree
                        (Diagram c' ([(CellType, Operand Text)], b) s') a
                      -> WriterT [(CellType, Text)] Identity ()
    mp2 (_ :< n) = do
        void $ mapDgA pure (tell.addressesOnly.fst) pure n
        traverse_ mp2 n

    addressesOnly s = [(t, a)|((t, Var a)) <- s]


-- runLadderTestXx
--     :: Bool
--     -> [ExtendedInstruction Int (V String) String]
--     -> [String]
--     -> [(Int, [(String, V String)])]
--     -> IO Bool
-- runLadderTestXx verbose prog signalNames testVect = do
--     undefined

runLadderTestX
    :: Bool
    -> LadderTest
    -> [ExtendedInstruction Int (Instruction (V String) String)]
    -> IO Bool
runLadderTestX verbose test@T01{} prog = do

    let allSigs = testVectSignals (testVect test)
--TODO select signals for display independently from signals for test evaluation
--     let displaySigs = allSigs -- or (watch test)
    when verbose $ print (here, allSigs)

--     runLadderTestXx verbose prog allSigs (testVect test)

    let xxy = evalTestVect''' prog allSigs (testVect test)

    when verbose $ print (here, xxy)
    let Right traces = xxy
    when verbose $ putStrLn $ unlines $ prettyTrace $ zip allSigs $ transpose traces

    let testTrace = getSignals (watch test) (testVect test) traces
    let passed = expected test == testTrace

    when verbose $ do
        print (here, testTrace)
        print (here, expected test)
        print (here, passed, if passed then "PASSED" else "FAILED" :: String)

    return passed

--------------------------------------------------------------------------------

ldUnpack1 :: Cofree (Diagram c op Text) a
         -> Cofree (Diagram c op String) a
ldUnpack1 (a :< n) = a :< fmap ldUnpack1 (mapDg id id unpack n)

--------------------------------------------------------------------------------

-- |return pragmas
parseOrDie5
    :: DeviceParser Text dev
    -> FilePath
    -> IO ( [String] -- leading pragmas
          , [(Maybe String, Cofree (Diagram Void dev String) DgExt)]
          )
parseOrDie5 devP path = do
    lxs <- lexFile path
    ast <- case parseOrDie2 devP $ dropWhitespace2 lxs of
                        Left  err -> fail err
                        Right x   -> return x
    let pragmas  = fmap unpack $ fmap mconcat <$> getLeadingPragmas $ dropPos lxs
    return (pragmas, ast)

parseOrDie2
    :: (MonadError String m, Monad m)
    => DeviceParser Text dev
    -> [[(DgExt, Tok Text)]]
    -> m [(Maybe String
        , Cofree (Diagram Void dev String) DgExt)]
parseOrDie2 devP lxs = do
    let blocks = labeledRungs lxs
    for blocks (\(lbl, p) -> (fmap unpack lbl,) <$> parseOrDie p)

    where
    -- |assuming comments and pragmas were filtered out
    parseOrDie lxs' = do
        case runLadderParser_ devP ladder lxs' of
            Right ast -> return $ ldUnpack1 ast
            Left  err -> throwError $ show (here, err)

--------------------------------------------------------------------------------

lexFile :: FilePath -> IO [[(DgExt, Tok Text)]]
lexFile file = do
    src <- TIO.readFile file
    case stripPos3 <$> runLexer src of
        Left  err -> fail $ show (here, err)
        Right x   -> return x

--------------------------------------------------------------------------------

pickPragma :: Text -> [[Text]] -> ([[Text]], [[Text]])
pickPragma key = partition f
    where
    f s = case s of
               x : _ | key' : _ <- T.words x -> key' == key
               _                             -> False

loadLadderTest :: FilePath -> IO (Maybe LadderTest, [[(DgExt, Tok Text)]])
loadLadderTest file = do
    x <- lexFile file
    let (pgms, _) = pickPragma "T01" $ getLeadingPragmas $ dropPos x
--     print (here, file, pgms)
--     let lang = filter (T.words ) pgms
--     "LANGUAGE"

--     let pgma = fmap (filter (/='\\') . unpack) $ fmap mconcat $ getPragma $ dropPos x
--     return (pgma >>= readMaybe, x)
    let pgms' = fmap (readMaybe . filter (/='\\') . unpack . T.concat) pgms
    let t = [ p | Just p : _ <- for pgms' pure ]
--     print (here, file,        pgms')
    return (t, x)

--------------------------------------------------------------------------------

istopo :: (a -> a -> Bool) -> [a] -> Bool
istopo dep (x : xs) = all (\y -> not $ dep x y) xs && istopo dep xs
istopo _   []       = True

istopoM :: (a -> a -> Bool) -> [a] -> Maybe a
istopoM dep (x : xs) = fst (pickFirst (dep x) xs) <|> istopoM dep xs
istopoM _   []       = Nothing

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


iscycle :: (a -> a -> Bool) -> a -> [a] -> Bool
iscycle dep x = go x
    where
    go a as = case depend of
                   [] -> False
                   _ | any (dep x) depend -> True --flip dep?
                   _ -> any (flip go indep) depend
        where
        (depend, indep) = partition (flip dep a) as

sameLine :: Cofree (Diagram c d s) DgExt -> Bool
sameLine n@((ln, _) :< _) = getAll $ foldMap (All.(ln==).fst) n
