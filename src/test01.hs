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
-- import Data.Bifunctor
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

collectSinks :: (MonadState (f a) m, Semigroup (f a),
            Applicative f) =>
            Cofree (Diagram continuation device label) a -> m ()
collectSinks (p :< Sink) = modify (<> pure p)
collectSinks (_ :< other) = for_ other collectSinks

collectNodes :: (MonadState (f a) m, Semigroup (f a),
            Applicative f) =>
            Cofree (Diagram continuation device label) a -> m ()
collectNodes (p :< Node w) = modify (<> pure p) *> for_ w collectNodes
collectNodes (_ :< other) = for_ other collectNodes

collectNodes' :: (Applicative f, Monoid (f a)) =>
                Cofree (Diagram continuation device label) a -> f a
collectNodes' ast = execState (collectNodes ast) mempty

checkDataDep :: (Foldable t, Eq a, Monad m, Monoid (m a)) =>
                t a -> Cofree (Diagram continuation device label) a -> m a
checkDataDep sinks x 
    | (p :< Node _) <- x, elem p sinks = return p 
    | otherwise                        = mempty

--------------------------------------------------------------------------------

data PP p m a = PP
    { ppPos :: p
    , ppNodes :: [p]
    , ppCont :: StateT (TraverseState p m a) m ()
    }

-- 'postponed' is appended when -see above
data TraverseState p m a = TraverseState
    { postponedUntil :: [PP p m a] -- location after which we can run cont, cont
    , unevaluatedNodes :: [p]
    , evaluatedSinks :: [p]
    }

-- traverseDiagram
--     :: (Ord pos, Eq pos, Monad m) =>
--     (a1
--         -> pos
--         -> Diagram continuation device label (Cofree (Diagram continuation device label) pos)
--         -> m a1)
--     -> a1
--     -> Cofree (Diagram continuation device label) pos
--     -> m (TraverseState pos m a2)
traverseDiagram emit q0 ast = execStateT (go q0 ast) (TraverseState [] unEvNodes [])

    where

    go q x@(p :< Node w) = do
        dataDeps <- getDataDeps w
        locationDeps <- getUnevaluatedAndNotPostponedNodesAt p
        case dataDeps <> locationDeps of
            [] -> do
                markNodeAsEvaluated p
                q' <- emit' q x
                runPostponed p
                for_ w (go q')
            deps -> do
                let r = maximum deps --postpone until most distant dependecy
                postpone x r q
    go q x@(p :< Sink) = do
        markSinkAsEvaluated p
        void $ emit' q x
        runPostponed p
    go q othr@(_ :< x) = emit' q othr >>= \q' -> for_ x (go q')

    emit' q (p :< x) = lift $ emit q p x

--look for unevaluted nodes on which node depends
--XXX why evaluatedSinks? why not all sinks?
--returns not yet evaluated dependencies
    getDataDeps w = do
        let deps = foldMap (checkDataDep sinks) w
        evaluated <- gets evaluatedSinks
        return $ deps \\ evaluated

    markSinkAsEvaluated p = modify \st -> st {evaluatedSinks=pure p <> evaluatedSinks st}
    markNodeAsEvaluated p = modify \st -> st {unevaluatedNodes=delete p (unevaluatedNodes st)}

    unEvNodes = execState (collectNodes ast) []

    --hate this hack :(
    getUnevaluatedAndNotPostponedNodes
        = (\\) <$> gets unevaluatedNodes <*> gets (foldMap ppNodes . postponedUntil)

    getUnevaluatedAndNotPostponedNodesAt p
        = filter (< p) <$> getUnevaluatedAndNotPostponedNodes

    postpone ast1 p qqq
        = modify \st
            -> st {postponedUntil = PP p (collectNodes' ast1) (go qqq ast1) : postponedUntil st}

    runPostponed p = do
        q <- gets postponedUntil
        let qq = filter ((p==).ppPos) q
        modify \st
            -> st { postponedUntil = deleteFirstsBy (on (==) ppPos) (postponedUntil st) qq}
        for_ qq ppCont

    sinks = execState (collectSinks ast) []

--------------------------------------------------------------------------------

-- emWrap :: (Eq pos, Show pos, Show q, Show qq, Show label, Monad m) => pos
--                       -> pos
--                       -> Diagram continuation (q, qq) label
--                            (Cofree (Diagram continuation1 device label1) pos)
--                       -> StateT (EmitState pos [String]) m pos
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
    --now `or` stack top with all `deps`
        for_ deps \d -> do
            bringToTop d d
            pop --first `OR` operand
            pop --second `OR` operand
--             emit [show (">>>>> OR", p, d)]
            emit [ EISimple IOr ]
            push p --intermediate result
        for_ (drop 1 w) \_ -> do
--             emit [show (">>>>> DUP")]
            emit [ EISimple IDup ]
            push p --now result of this node is on stack
    go  Sink             = do
        nodes <- gets esNodes
        if elem p nodes
        then bringToTop q p
        else do
            pop
--             emit [show (">>>>> DROP", p)]
            emit [ EISimple IDrop ]
    go (Source _a       ) = do
        push p
--         emit [show (">>>>> PUSH #1")]
        emit [ EISimple ILdOn ]
    go  End              = do
        pop -- ????
--         emit [show (">>>>> DROP")]
        emit [ EISimple IDrop ]
    go (Device device _a) = do
        bringToTop q q
        pop
--         emit [show (">>>>> EVAL", device)]
        emit $ emitDevice device
        push p
    go (Jump   label  ) = do
        bringToTop q q
--         emit [show (">>>>> CJMP", label)]
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
--                 emit [show (">>>>> PICK ", length pre)]
                emit [ EISimple (IPick (length pre)) ]
                modify \st -> st { esStack = pre <> ((v, True) : rest) }
        push name

    push v = modify \st -> st { esStack = (v, False):esStack st}

    pop = do
        stk <- gets esStack
        case stk of
            _:stk' -> modify \st -> st { esStack = dropWhile snd stk' }
            [] -> undefined


data EmitState p w = EmitState
    { esStack :: [(p, Bool)] -- flag if used and can dropped
    , esSinks :: [p]
    , esNodes :: [p]
    , esCode :: w
    }

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
    let (_, u) = runState
                    (traverseDiagram
                        (emWrap (fmap EISimple . doDevice))
                        p0
                        ast
                        )
                    (EmitState
                        []
                        (execState (collectSinks ast) [])
                        (execState (collectNodes ast) [])
                        []
                        )
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

test1 lxs = do
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
    putStrLn $ "═════╪" ++ replicate 80 '═'
    for_ (zip [1::Int ..] (T.lines src)) \(i, ln) ->
        printf "%4i ┊%s\n" i ln
    putStrLn $ "═════╧" ++ replicate 80 '═'
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
--                         sinks ast1
                        (_, u) <- runStateT
                            (traverseDiagram
--                                 (emWrap (pure.show))
                                (emWrap (\_ -> []))
                                p0
                                ast1
                                )
                            (EmitState
                                []
                                (execState (collectSinks ast1) [])
                                (execState (collectNodes ast1) [])
                                []
                                )
                        for_ ((esCode u) :: [ExtendedInstruction T.Text Int Int]) print
                        print $ length $ esStack u

--                         u <- traverseDiagram emitPrint 0 ast1
--                         print $ length $ postponedUntil u
--                         print $ length $ unevaluatedNodes u
--                         print $ length $ evaluatedSinks u
                        return ()
    where
--     deviceThing = wrapDevice3 (pure . I) (pure . A)
    deviceThing = wrapDeviceSimple
