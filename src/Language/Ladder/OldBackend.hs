#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.OldBackend where

import Data.List
import Data.Foldable
import Control.Monad.Writer.Strict
import Data.Tuple
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Char (toUpper)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bifunctor

import Language.Ladder.DiagramParser
import Language.Ladder.LadderParser
import Language.Ladder.Utils
import Language.Ladder.Interpreter
import Language.Ladder.Analysis

import Language.Ladder.Simple

--------------------------------------------------------------------------------

verbose = False

--------------------------------------------------------------------------------

-- |Return list of annotations (usually positions) of 'Sink' nodes
stubs
    :: Cofree (Diagram c d s) p
    -> [p]
stubs (p :< a) = f a
    where
    f (Source a)   = stubs a
    f  Sink        = [p]
    f  End         = []
    f (Device d a) = stubs a
    f (Jump s)     = []
    f (Node a)     = foldMap stubs a
    f (Conn c    ) = [] --XXX i am not sure !!!
    f (Cont c   a) = []

ldlines'' :: [Cofree (Diagram () d s) DgExt] -> [Cofree (Diagram DgExt d s) DgExt]
-- ldlines'' = sortOn (\(p:<_)->p) . foldMap ldlines'
ldlines'' = foldMap ldlines'

ldlines' :: Cofree (Diagram () d s) DgExt -> [Cofree (Diagram DgExt d s) DgExt]
-- ldlines' tr = let (a, b) = ldlines tr in a : b
ldlines' = toList . ldlines

ldlines
    :: Cofree (Diagram () d s) DgExt
--NonEMpty seem like better option here, foldabale etc
--     -> ( Cofree (Diagram DgExt d s) DgExt
--        , [Cofree (Diagram DgExt d s) DgExt]
--        )
    -> NonEmpty (Cofree (Diagram DgExt d s) DgExt)
ldlines tr = let (a, b) = f tr in a :| b
    where

    f (p@(ln, _) :< Node l) = (p :< Node (a' ++ fmap (const (p :< Conn p)) b) , as'')
        where

        as'' = concat as' ++ fmap (\nn@(pp:<_) -> pp :< Cont p nn) b' ++ concat bs'

        (a, b) = partition (\((ln', _) :< _) -> ln'==ln) l
        (a', as') = unzip $ fmap f a
        (b', bs') = unzip $ fmap f b
    f (p :< Source a)   = bimap ((p:<) . Source) id $ f a
    f (p :< Sink)       = (p :< Sink, [])
    f (p :< End)        = (p :< End, [])
    f (p :< Device d a) = bimap ((p:<) . Device d) id $ f a
    f (p :< Jump s)     = (p :< Jump s, [])
    f _                 = error here

--------------------------------------------------------------------------------

nodeTable :: [(p, [p])] -> [(p, p)]
nodeTable = foldMap (\(x, xs) -> (x, x) : fmap (,x) xs)

generateStk1
    :: Cofree (Diagram () (Dev String) String) DgExt
    -> IO [ExtendedInstruction String (V String) String]
generateStk1 ast = (EISimple <$>) <$> generateStk ast

generateStk
    :: Cofree (Diagram () (Dev String) String) DgExt
    -> IO [Instruction (V String) String]
generateStk ast' = do
    let Right ast = parseOpsM ast'

    when verbose $ print (here, "-----------------------")

    --chop
    let Just x1 = forest ast

    --collect stubs (per each forest tree)
    let x1' -- :: [([DgExt], Cofree (Diagram c (Op Operand String) String) DgExt)]
            = fmap (\x -> (stubs x, x)) x1
    --merge neighbouring nodes
    let q -- :: [(([DgExt], [(DgExt, [DgExt])]), Cofree (Diagram DgExt (Op Operand String) String) DgExt)]
            = fmap (\(stbs, tre) -> let (nds, tre') = merge' tre
                in ((stbs, nds), tre')
                    --this is result - stubs in subtree, merged nodes and new tree
                    ) x1'

    let allStubs = foldMap (fst . fst) q --aka sinks
    let nodesMerged :: [(DgExt, [DgExt])] = foldMap (snd . fst) q
    let allNodes = nub $ fmap fst nodesMerged ++ foldMap snd nodesMerged
    let sinksLeadingToNodes = filter (flip elem allNodes) allStubs --aka sinks
    when verbose $ do
        print (here, "allNodes:", allNodes)
        print (here, "nodesMerged:", nodesMerged)
        print (here, "sinksLeadingToNodes:", sinksLeadingToNodes)
    let oldNodeToNewNode = nodeTable nodesMerged
    let nodeToSink
            = fmap (\p -> (fromJust $ lookup p oldNodeToNewNode, p)) sinksLeadingToNodes
    when verbose $ print (here, "nodeToSink:", nodeToSink)

    when verbose $ do
        print (here, "-----------------------")
        for_ q $ \((stubs, _), tr) -> do
            print $ filter (flip elem allNodes) stubs
            print tr
        print (here, "-----------------------")

    let subTrees = fmap (\((stubs, _), tr) -> (stubs, tr)) q
    execWriterT $ generate tell nodeToSink subTrees

--------------------------------------------------------------------------------

--XXX XXX XXX prob want new base functor, without 'End' (and without 'Source'?)
-- and distinguishing 'Sink' and 'Stub'

--only node may have multiple successors
--stub(sink) may have one or zero successors
--i should make newtype for nodeToSink elements

--TODO allow ading annotations in 'emit'

generate ::
    Monad m0 =>
    Show b0 =>
    Show s1 =>
    Show s0 =>
    Eq b0 =>
    Eq c =>
    Show c =>
    ([Instruction w String] -> m0 ())
    -> [(b0, b0)]
    -> [([b0], Cofree (Diagram c (Op s0 (Operand String)) s1) b0)]
    -> m0 ([b0], [([b0], Cofree (Diagram c (Op s0 (Operand String)) s1) b0)])

generate emit nodeToSink asts = go ([], asts)

    where

    sinkToNode = nub $ fmap swap nodeToSink --do i need nub? i think yes

    go (stack, []) = do
        return (stack, [])
--    go stack (p :< a) xs -- get rid of stubs
    go (stack, (stubs, p :< a) : xs) = f stack a

        where

        f stk (Source b)       = do
            emit [ILdOn]
            go (p:stk, (stubs, b) : xs)
        f (_:stk) Sink         = do
            case lookup p sinkToNode of --XXX here is argument for distinguishing 'Sink' and 'Stub'
                Just _  -> return (p:stk, xs) --put value back under name with which is referenced
                Nothing -> do
                    emit [IDrop]
                    return (stk, xs)
        f stk End              = go (stk, xs)
        f (x:stk) (Device d b) = do
            case d of
                 And (Var addr) -> emit [ILdBit addr, IAnd]
                 AndN (Var addr) -> emit [ILdBit addr, INot, IAnd]
                 St (Var addr)  -> emit [IStBit addr]
                 _ -> error here
            go (p:stk, (stubs, b):xs)
        f stk (Jump s)         = error here --later
        f (_:stk) (Node b)     = do
            --at least one value on stack
            --look for stubs coinciding with this node
            let needToEvalFirst1{-node positions-}
                    = filter ((p==).fst) nodeToSink

            (stk', xs') <-
                foldlM
                    step
                    (stk, xs)
                    needToEvalFirst1

            let dups = replicate (length b - 1) p
--             liftIO $ print (here, ">>>>>>>>>", length b)
            for_ dups $ const $ emit [IDup]

            foldlM
                (\(stk'', xs'') tr
                    -> go (stk'', ([{-don't care about stub list here-}], tr) : xs'')
                    )
                (dups ++ stk', xs')
                b
            where
            step (stk', xs') (_, stubP) = do
                case findIndex (stubP==) stk' of
                     Just i -> do
                         case i of
                            0 -> return ()
                            _ -> emit [IPick i]
                         emit [IOr]
                         return (stubP:stk', xs')
                     Nothing ->
                        case partition (elem stubP .fst) xs' of
                          --stub must appear in exactly one subtree
                            ([tr@(_, _:< _)], rest) -> do
                                s@(stk'', _) <- go (stk', tr : rest)
                                --now fetch and or??
                                case findIndex (stubP==) stk'' of --check latest stack
                                    Just i ->
                                        case i of
                                            0 -> return ()
                                            _ -> emit [IPick i]
                                    Nothing
                                        -> error $ show (here, stubP, stk'') --should not happen
                                emit [IOr]
                                return s
                            other -> error $ show (here, other) --should not happen

--         f (pp:stk) (Conn c) = do
--             return (c:stk, xs)
--         f stk (Cont stubP b) = do
--             case findIndex (stubP==) stk of
--                 Just i -> do
--                     case i of
--                         0 -> return ()
--                         _ -> emit [IPick i]
-- --                     return (stubP:stk, xs)
--                     go (stubP:stk, (stubs, b) : xs)
-- 
--                 Nothing -> error here
-- --             undefined

        f stk n = error $ show (here, stk, n)

--------------------------------------------------------------------------------

#if 1
data Dev t = Dev !(DevType t) ![Operand t]
    deriving (Show, Eq, Functor)

parseSimpleDevice :: DeviceParser Text (Op String (Operand String))
parseSimpleDevice d
    = case lookup d (devices pure pure) of
        Just dd@(DDesc _name ty _impl)
            -> Right (if length ty > 1 then Mandatory else None
                    , \ops -> parseOp $ fmap unpack $ Dev d ops)
        Nothing -> Left "device type unknown"

--------------------------------------------------------------------------------

data Op s n
    = And       n -- wire out <- wire in and memory cell
    | AndN      n
    | Ld          -- ^ set wire state same as argument
    | On          -- ^ set wire state to #on
    | St        n
    | StN       n -- | StOn | StOff
    | LdP       n -- rising edge detect
    | LdN       n -- falling edge detect
    | Jmp s
    | Cmp CmpOp n n
--     | OpTrap
    deriving (Show, Eq) -- , Functor)

data CmpOp = Lt | Gt | Lte | Gte | Eq | NEq
    deriving (Show, Eq)

--------------------------------------------------------------------------------

mapSimpleOpOperandA
    :: (Applicative m)
    => (CellType -> t -> m n)
    -> Op s t
    -> m (Op s n)
mapSimpleOpOperandA doOperand = f
    where
    f (And    a  ) =        And    <$> doOperand Bit a
    f (AndN   a  ) =        AndN   <$> doOperand Bit a
    f (St     a  ) =        St     <$> doOperand Bit a
    f (StN    a  ) =        StN    <$> doOperand Bit a
    f (Cmp op a b) =        Cmp op <$> doOperand Word a <*> doOperand Word b
    f  Ld          = pure   Ld
    f (LdP    a  ) =        LdP    <$> doOperand TwoBits a
    f (LdN    a  ) =        LdN    <$> doOperand TwoBits a
    f  On          = pure   On
    f (Jmp s)      = pure $ Jmp s

emitBasicDevice
    :: (Show address) -- , Show op)
    => Op op (Operand address)
    -> [Instruction word address]
emitBasicDevice d
    = case d of
        And  (Var addr)  -> [ILdBit addr, IAnd]
        AndN (Var addr)  -> [ILdBit addr, INot, IAnd]
        St   (Var addr)  -> [IStBit addr]
        StN  (Var addr)  -> [INot, IStBit addr, INot]
        _                -> error here -- $ show (here, d)

parseOpsM
    :: Cofree (Diagram c (Dev String) s) p
    -> Either String (Cofree (Diagram c (Op s (Operand String)) s) p)
parseOpsM = mapOpsM parseOp

parseOp
    :: Dev String
    -> Either String (Op s (Operand String))
parseOp = f
    where
    f (Dev (Coil_ op) arg) = case (fmap toUpper op, arg) of
        (" ", [n]   ) -> pure $ St n
        ("/", [n]   ) -> pure $ StN n
        ("R", [n]   ) -> pure undefined
        ("S", [n]   ) -> pure undefined
        _               -> Left "unknown coil type"
    f (Dev (Contact_ op) arg) = case (fmap toUpper op, arg) of
        (" ", [n]   ) -> pure $ And  n
        ("/", [n]   ) -> pure $ AndN  n
        (">", [a, b]) -> pure $ Cmp Gt a b
        ("<", [a, b]) -> pure $ Cmp Lt a b
        ("P", [n]   ) -> pure $ LdP n
        ("N", [n]   ) -> pure $ LdN n
        _               -> Left "unknown contact type"
#endif
