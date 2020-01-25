{-# OPTIONS_GHC -Wunused-imports  -Wall #-}
{-# LANGUAGE CPP, TupleSections, FlexibleContexts, ScopedTypeVariables, BlockArguments #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Foldable
import Data.Void

-- import Language.Ladder.Zipper
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Simple
import Language.Ladder.Interpreter

import Language.Ladder.Utils

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.Except
-- import qualified Data.Map as M
import Data.Bifunctor
import Data.List
import Data.Function
-- import Data.Proxy
import Data.Text (Text, unpack)
import qualified Data.Text as T --(Text, lines)
import Text.Printf
import Data.Functor.Identity

import Data.Traversable

import System.Console.ANSI.Types
import System.Console.ANSI.Codes

--------------------------------------------------------------------------------

checkDataDep :: (Foldable t, Eq a, Monad m, Monoid (m a)) =>
                t a -> Cofree (Diagram c d l) a -> m a
checkDataDep sinks x 
    | (p :< Node _) <- x, elem p sinks = return p 
    | otherwise                        = mempty


collectNodesAndSinks :: Cofree (Diagram c d l) a -> ([a], [a])
collectNodesAndSinks ast = execState (go ast) ([], [])
    where   go (p :< Node w) = modify (first (p:)) *> for_ w go
            go (p :< Sink  ) = modify (second (p:))
            go (_ :< other ) = for_ other go

--------------------------------------------------------------------------------

{-
(1) processing top->down, left->right
(2) processing is postponed (made out of abovementioned order)
    in horizontal direction when node is fed from yet unprocessed device
    in vertical direction when yet unprocessed line has to be stepped over
        vertical means when going down the rail from node

(3) how to recognize when i can complete postponed work?
(4) postponing happens only at nodes

(5) when node has unprocessed predeccessor?
    when it has outgoing branch with node that coincides on position with some 'Sink'

(6a) both position-priority and data-dependency need to be checked only at 'Node' element
(6b) posponed actions are checked after 'Sink' (data and position deps)
    or 'Node' (position deps) is evaluated
-}

data PP p m a = PP
    { ppPos   :: p
    , ppNodes :: [p]
    , ppCont  :: StateT (TraverseState p m a) m ()
    }

data TraverseState p m a = TraverseState
    { tsPostponed      :: [PP p m a] -- location after which we can run cont, cont
    , unevaluatedNodes :: [p]
    , evaluatedSinks   :: [p]
    }

traverseDiagram
    :: (Ord p, Monad m)
    => (a -> p -> Diagram c d l (Cofree (Diagram c d l) p) -> m a)
    -> (a -> Cofree (Diagram c d l) p -> m ())
    -> a
    -> Cofree (Diagram c d l) p
    -> m [p] -- (TraverseState p m a)
traverseDiagram emit post q0 ast
    = (fmap ppPos.tsPostponed) <$> execStateT (go q0 ast) (TraverseState [] unEvNodes [])
--     =  execStateT (go q0 ast) (TraverseState [] unEvNodes [])

    where

    go q x@(p :< Node w) = do
        dataDeps     <- getDataDeps w
        locationDeps <- getUnevaluatedAndNotPostponedNodesAt p
        case dataDeps <> locationDeps of
            []   -> markNodeAsEvaluated p *> eval q x
            deps -> postpone x (maximum deps) q --postpone until most distant dependecy
    go q x@(p :< Sink) = markSinkAsEvaluated p *> eval q x
    go q other = eval q other

    eval q (p :< w) = do
        q' <- lift (emit q p w)
        runPostponed p
        for_ w (go q')

--look for unevaluted nodes on which node depends
--returns not yet evaluated dependencies
    getDataDeps w = do
        let deps = foldMap (checkDataDep sinks) w
        evaluated <- gets evaluatedSinks
        return $ deps \\ evaluated

    markSinkAsEvaluated p = modify \st -> st {evaluatedSinks = p : evaluatedSinks st}
    markNodeAsEvaluated p = modify \st -> st {unevaluatedNodes = delete p (unevaluatedNodes st)}

    --hate this hack :(
    getUnevaluatedAndNotPostponedNodes
        = (\\) <$> gets unevaluatedNodes
               <*> gets (foldMap ppNodes . tsPostponed)

    getUnevaluatedAndNotPostponedNodesAt p
        = filter (< p) <$> getUnevaluatedAndNotPostponedNodes

    postpone x until q = do
        let stub = PP until (fst (collectNodesAndSinks x)) (go q x)
        modify \st -> st {tsPostponed = stub : tsPostponed st}
        lift $ post q x

    runPostponed p = do
        (now, later) <- gets (partition ((p==).ppPos) . tsPostponed)
        modify \st -> st { tsPostponed = later }
        for_ now ppCont

    (unEvNodes, sinks) = collectNodesAndSinks ast

--------------------------------------------------------------------------------

emWrap
    :: (Eq p)
    => (d -> [ExtendedInstruction l k adr])
    -> p
    -> p
    -> Diagram c d l (Cofree (Diagram c d l) p)
    -> StateT
        (StackEmitState p [ExtendedInstruction l k adr])
        (Either String)
        p --TODO use MonadState constraint
emWrap emitDevice q p x = go x *> pure p
    where

    go (Node   []      ) = do
--         pop
--         liftIO $ print (">>>>> DROP")
        return () -- "optimization"
    go (Node   w       ) = do
        sinks <- gets esSinks
        let deps = (foldMap (checkDataDep sinks) w) ++ [] -- ugh
        bringToTop q p --renaming to current node - pass through
        for_ deps \d -> do --now `or` stack top with all `deps`
            bringToTop d d
            pop --first `OR` operand
            pop --second `OR` operand
            emit [ EISimple IOr ]
            push p --intermediate result
        for_ (drop 1 w) \_ -> do
            emit [ EISimple IDup ]
            push p --now result of this node is on stack
    go  Sink             = do
        nodes <- gets esNodes
        if elem p nodes
        then bringToTop q p
        else do
            pop
            emit [ EISimple IDrop ]
    go (Source _a       ) = do
        push p
        emit [ EISimple ILdOn ]
    go  End              = do
        pop -- ????
        emit [ EISimple IDrop ]
    go (Device device _a) = do
        bringToTop q q
        pop
        emit $ emitDevice device
        push p
    go (Jump   label  ) = do
        bringToTop q q
        emit [ EIJump label ]
        pop
    go (Cont   _continuation _a) = undefined
    go (Conn   _continuation) = undefined

    emit s = modify \st -> st { esCode = esCode st ++ s }

    bringToTop pp name = do
        stk <- gets esStack
        case break ((==pp) . fst) stk of
            (_pre, []) -> undefined --not found
            ([], _) -> pop --pop current and push it with new name
            (pre, (v, _) : rest) -> do
                emit [ EISimple (IPick (length pre)) ]
                modify \st -> st { esStack = pre <> ((v, True) : rest) }
        push name

    push v = modify \st -> st { esStack = (v, False) : esStack st }

    pop = do
        stk <- gets esStack
        case stk of
            _:stk' -> modify \st -> st { esStack = dropWhile snd stk' }
            [] -> undefined


data StackEmitState p w = StackEmitState
    { esStack :: [(p, Bool)] -- flag if used and can dropped
    , esSinks :: [p]
    , esNodes :: [p]
    , esCode :: w
    }

--------------------------------------------------------------------------------

--TODO TODO TODO

data Setup t lxs ast d code m = Setup
    { sLexer        :: t -> m lxs
    , sDeviceParser :: t -> m d
    , sParser       :: (t -> m d) -> lxs -> m ast
    , sDeviceEmit   :: d -> m code
    , sCodeGen      :: ast -> m code
    }

accSetup = Setup
    { sLexer        = undefined
    , sDeviceParser = undefined
    , sParser       = undefined
    , sDeviceEmit   = undefined
    , sCodeGen      = undefined
    }

{-
data Failure = Lexing | Parsing | CodeGen | ...
:: Setup -> m Failure
-}

--------------------------------------------------------------------------------

data AI r w a
    = AITrap --invoke debugger
    | AILdOn
    | AILdReg !r
    | AIStReg !r

    | AILdBit !a
    | AIStBit !a

    | AIAnd   !r
    | AIOr    !r
    | AINot

    | AILdCnA !w
    | AILdM
    | AIStM

    | AIEq
    | AILt
    | AIGt
    deriving (Show, Eq)

accuEmit
    :: (Eq p, Show p, Show l)
    => p
    -> p
    -> Diagram c d l (Cofree (Diagram c d l) p)
    -> StateT
        (AccuEmitState p [String])
        (Either String)
        p
accuEmit q p x = go x *> pure p
    where

    go (Node   []      ) = do
        accSet p -- "optimization"
    go (Node   w       ) = do
        sinks <- gets aesSinks
        let deps = (foldMap (checkDataDep sinks) w) ++ [] -- ugh
        getValue q
        for_ deps \d -> do
            r <- getFromRegs d
            accEmit [ "or $" ++ show r ]
        accSet p
--         when (length w > 1) do accSpill (length w) p
        unless (null w) do accSpill (length w) p
    go  Sink             = do
        nodes <- gets aesNodes
        if elem p nodes
        then do
            accSet p
            accSpill 1 p
        else return ()
--         accSet p
--         accSpill 1 p --TODO check if required
    go (Source _a       ) = do
        accEmit [ "ld #1" ]
        accSet p
    go  End              = do
        return ()
    go (Device device _a) = do
        getValue q
        accEmit [ "eval ??" ++ " // " ++ show p ]
        accSet p
    go (Jump   label  ) = do
        getValue q
        accEmit [ "cjmp " ++ show label ]
    go (Cont   _continuation _a) = undefined
    go (Conn   _continuation) = undefined

    --find value in registers
    getValue pp = do
        accu <- gets aesAccu
        if accu == pp
        then return ()
        else getFromRegs pp >>= load

    load r = accEmit [ "ld $" ++ show r ]

    getFromRegs pp = do
        rf <- gets aesRegisters
        case break ((==pp) . fst) rf of
            (_pre, []) -> undefined --not found
            (pre, (ppp, (v, uses)) : rest) -> do
                let rf' = case uses of
                            1 -> pre ++ rest
                            _ -> pre ++ (ppp, (v, uses - 1)) : rest
--                 let rf' = rf --TODO
                modify \st -> st { aesRegisters = rf'}
                return v

accEmit s = modify \st -> st { aesCode = aesCode st ++ s }
accSet q = modify \st -> st { aesAccu = q }

accSpill :: Int -> p -> StateT
        (AccuEmitState p [String])
        (Either String) ()
accSpill uses name = do
    rf   <- gets aesRegisters

    let rUseNumber = snd . snd
    case break ((==0) . rUseNumber) rf of
        (_pre, []) -> do --free register not found
            modify \st -> st { aesRegisters =  rf ++ [(name, (length rf, uses))] }
            accEmit [ "st $" ++ show (length rf) ] 
        (pre, (ppp, (v, uses)) : rest) -> do
            modify \st -> st { aesRegisters =  pre ++ (name, (v, uses)) : rest }
            accEmit [ "st $" ++ show v ] 

--     modify \st -> st { aesRegisters =  rf ++ [(name, (length rf, uses))] }
--     accEmit [ "st $" ++ show (length rf) ] 

accuPost
    :: a
    -> Cofree (Diagram c d l) a
    -> StateT (AccuEmitState a [String]) (Either String) ()
accuPost q (_ :< Node w@(_:_)) = accSpill (length w) q
accuPost _  _                  = return () --not needed, forget it



data AccuEmitState p w = AccuEmitState
    { aesAccu      :: p
    , aesSinks     :: [p]
    , aesNodes     :: [p]
    , aesRegisters :: [(p, (Int, Int))]
    , aesCode      :: w
    }

blargh ast@(q0 :< _)
    = runStateT
        (traverseDiagram accuEmit accuPost q0 ast)
        (AccuEmitState q0 sinks nodes [] [])
    where
    (nodes, sinks) = collectNodesAndSinks ast --do i need this?

--------------------------------------------------------------------------------

generateStk2THISISTOOMUCH
    :: (Show addr, Show word, Show lbl, Eq lbl, MonadError String m, Monad m)
    => (dev -> Either String x) --TODO swap (Either String) for m
    -> (x -> [Instruction word addr])
    -> [(Maybe lbl, Cofree (Diagram Void dev lbl) DgExt)]
    -> m [ExtendedInstruction Int word addr]
generateStk2THISISTOOMUCH doOp emitDev ast = do
    ast'   <- for ast (traverse (mapOpsM (liftEither . doOp))) --FIXME remove liftEither
    ast''  <- for ast' (traverse (generateStkOMFG emitDev))
    ast''' <- liftEither $ resolveLabels ast'' 
    return ast'''

-- generateStk2' 
generateStkOMFG
    :: (Show lbl, Eq lbl
    , Show addr
    , Show word
--     , Show device
    , Monad m
    )
    => (device -> [Instruction word addr])
    -> Cofree (Diagram Void device lbl) DgExt
    -> m [ExtendedInstruction lbl word addr]
generateStkOMFG doDevice ast@(p0 :< _) = do
    let (nodes, sinks) = collectNodesAndSinks ast
    let Right (_, u) = runStateT
                    (traverseDiagram (emWrap (fmap EISimple . doDevice))
                        (\_ _ -> pure ()) p0 ast)
                    (StackEmitState [] sinks nodes [])
    return $ esCode u

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
    -> m [ExtendedInstruction Int (V String) String]
compileForTest03 ast = do
    (++ [EIReturn]) <$> generateStk2THISISTOOMUCH pure emitDevice03 ast
-- compileForTest03 = undefined

-- ehlo
--     :: [(Maybe String
--             , Cofree (Diagram Void 
--                     (([(CellType, Operand Text)], DeviceImpl (V String) String))
--                     String) DgExt)]
--     -> IO ()
ehlo ast = do
--     when verbose $ print here
    prog <- either fail pure $ compileForTest03 ast
    let memSlots :: [(CellType, Text)] = nub $ execWriter $ traverse_ (traverse_ mp2) ast
--     print (here, memSlots, "<<<<<"::String)
--     print (here, memSlotsToTestVector 4 memSlots, "<<<<<"::String)

--     when verbose $ 
    do
        putStrLn "---------------------------"
        for_ prog print
        putStrLn "---------------------------"
--     runLadderTestX verbose test prog

    where
    mp2 :: Cofree
                        (Diagram c' ([(CellType, Operand Text)], b) s') a
                      -> WriterT [(CellType, Text)] Identity ()
    mp2 (_ :< n) = do
        void $ mapDgA pure (tell.addressesOnly.fst) pure n
        traverse_ mp2 n

    addressesOnly s = [(t, a)|((t, Var a)) <- s]

--------------------------------------------------------------------------------

test1 ast1 = do
    let Right (_, st) = blargh ast1
    print (here)
    for_ (aesCode st) print
    return ()

test2 lxs = do

    case runLadderParser_ (wrapDevice3 (pure . I) (pure . A)) ladderLiberal lxs of
        Left  err -> print (here, err)
        Right ast@(p0 :< _) -> do
            print here
            ehlo [(Nothing, ast)]
--             for_ ((esCode u) :: [ExtendedInstruction T.Text Int Int]) print
--             for_ (esCode u) print
--             print $ length $ esStack u
            return ()

implSimple :: ([(CellType, Operand T.Text)], DeviceImpl (V addr0) addr0)
             -> [ExtendedInstruction T.Text word0 address0]
implSimple = undefined

niceSrc file src = do
    putStrLn $ "     ┊ " ++ file
--     putStrLn $ "═════╪" ++ replicate 80 '═'
    putStrLn $ "═════╪" ++ concat (replicate 7 "════╤════╦")
    for_ (zip [1::Int ..] (T.lines src)) \(i, ln) ->
        printf "%4i ┊%s\n" i ln
--     putStrLn $ "═════╧" ++ replicate 80 '═'
    putStrLn $ "═════╧" ++ concat (replicate 7 "════╧════╩")
-- putStrLn  $ setSGRCode [SetItalicized True] ++ "hello"
-- putStrLn  $ setSGRCode [Reset] ++ "hello"

main :: IO ()
main = do
    [file] <- getArgs
    src <- TIO.readFile file
    case stripPos3 <$> runLexer src of
        Left err -> TIO.putStrLn err
        Right lxs -> do

            let lxs' = dropWhitespace2 lxs
            let blocks = labeledRungs lxs'

            print (here, "--------------------------------------------------")
--             TIO.putStrLn src
            niceSrc file src
            print (here, "--------------------------------------------------")

            forM_ blocks $ \(lbl, lxs'') -> do
                print (here, lbl)
                let zp = mkDgZp lxs''
                for_ (toList zp) (print . (here,))


                test2 lxs''


                case runLadderParser deviceThing ladderLiberal lxs'' of
                    Left err -> print (here, err)
                    Right (ast1@(p0 :< _), zp1) -> do
                        print (here, "--------------------------------------------------")
                        for_ (toList zp1) (print . (here,))
--                         print (here, "--------------------------------------------------")
--                         print (here, ast1)
--                         putStrLn ""
                        print (here, "--------------------------------------------------")
                        putStrLn ""
                        print (here, toList ast1)
                        print (here, nub $ toList ast1)
                        test1 ast1
--                         (_, u) <- runStateT
--                             (traverseDiagram
-- --                                 (emWrap (pure.show))
--                                 (emWrap (\_ -> []))
--                                 p0
--                                 ast1
--                                 )
--                             (EmitState
--                                 []
--                                 (execState (collectSinks ast1) [])
--                                 (execState (collectNodes ast1) [])
--                                 []
--                                 )
--                         for_ ((esCode u) :: [ExtendedInstruction T.Text Int Int]) print
--                         print $ length $ esStack u

--                         u <- traverseDiagram emitPrint 0 ast1
--                         print $ length $ postponedUntil u
--                         print $ length $ unevaluatedNodes u
--                         print $ length $ evaluatedSinks u
                        return ()
    where
--     deviceThing = wrapDevice3 (pure . I) (pure . A)
    deviceThing = wrapDeviceSimple
