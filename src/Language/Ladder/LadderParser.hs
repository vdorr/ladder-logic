#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Ladder.LadderParser
--     ( Diagram(..)
--     , Operand(..)
--     , DevType(..)
--     , DevOpFlag(..)
--     , LdP
--     , runParser
--     , ladder
--     , ladderLiberal
-- -- *for testing only
--     , box001
--     )
    where

import Prelude hiding (fail)
import Control.Applicative --hiding (fail)
import Control.Monad hiding (fail)
import Data.Foldable
import Data.Void
import Control.Monad.State hiding (fail)

import Language.Ladder.Utils
import Language.Ladder.Lexer
import Language.Ladder.DiagramParser
import Language.Ladder.Types

--------------------------------------------------------------------------------

-- |Contact operand, located above or below
data Operand address
    = Var !address   -- ^name of memory location
    | Lit !Int       -- ^integer literal, usually allowed only below contact
    deriving (Show, Eq, Functor)

data DevType t
    = Coil_    !t
    | Contact_ !t
    deriving (Show, Eq, Functor)

--------------------------------------------------------------------------------

data LdPCtx m text device = LdPCtx
    { ctxMkDev :: !(DevType text -> m (DevOpFlag, [Operand text] -> m device))
    }

data DevOpFlag = None | Optional | Mandatory
--     deriving (Show, Eq)

type LdP device text
    = SFM (DgPState (LdPCtx (Either String) text device) (Tok text))

--FIXME rename!!!!!!!!!!!!!!!!
type DeviceParser t d
    = DevType t -> Either String (DevOpFlag, [Operand t] -> Either String d)

runParser
    :: DeviceParser t d
    -> LdP d t a
    -> [[(DgExt, Tok t)]]
    -> Either String (a, Dg (Tok t))
runParser mkDev p s
--     = (psStr <$>) <$> applyDgp p (mkDgZp (dropWhitespace2 s)) (LdPCtx mkDev)
    = fst <$> applyDgp p' (mkDgZp (dropWhitespace2 s)) (LdPCtx mkDev)
    where
    p' = do
        x <- p
        (x,) <$> getStream
-- TODO parser that:
-- accepts untreated (not split to line, with all whitespce) token stream
-- handles labels

--------------------------------------------------------------------------------

ladder :: LdP d t (Cofree (Diagram Void d t) DgExt)
ladder
    = setDir goDown
    *> (ann <$> currentPos <*> fmap Source vline')
    <* dgIsEmpty

-- like 'ladder' but do not check if all lexemes consumed
ladderLiberal :: LdP d t (Cofree (Diagram Void d t) DgExt)
ladderLiberal
    = setDir goDown
    *> (ann <$> currentPos <*> fmap Source vline')

--------------------------------------------------------------------------------

number :: LdP d t (Operand t)
number = eat >>= \case  Number n -> return $ Lit n
                        _        -> lift $ Left "expected number"

cross :: LdP d t ()
cross = eat >>= \case   Cross -> return ()
                        _     -> lift $ Left "expected '+'"

name :: LdP d t t
name = eat >>= \case    Name lbl -> return lbl
                        _        -> lift $ Left "expected name"

hline :: LdP d t Int
hline = eat >>= \case   HLine _ vl -> return vl
                        _         -> lift $ Left "expected horizontal line"
    
vline :: LdP d t ()
vline = eat >>= \case   VLine -> return ()
                        _     -> lift $ Left "expected vertical line"

coil' :: LdP d t (DevType t)
coil' = eat >>= \case   Coil f -> return $ Coil_ f
                        _      -> lift $ Left "expected coil"

contact' :: LdP d t (DevType t)
contact' = eat >>= \case    Contact f -> return $ Contact_ f
                            _         -> lift $ Left "expected contact"

jump :: LdP d t (Cofree (Diagram c d t) DgExt)
jump = do
    p <- currentPos
    eat >>= \case   Jump' lbl -> return $ p :< Jump lbl
                    _         -> lift $ Left "expected jump"

--------------------------------------------------------------------------------

variable :: LdP d t (Operand t)
variable = Var <$> name

operand :: LdP d t (Operand t)
operand = variable <|> number

--------------------------------------------------------------------------------

withOperands :: LdP d t (DevOpFlag, a)
              -> LdP d t ([Operand t], a)
-- withOperands3 p = below (above_ p operand) optOper
withOperands deviceParser
        =        operand
        `above'` deviceParser
        `below`  optOper
    where
    above' = flip above_
    optOper ((Mandatory, a), op) = ((,a).(op:)) <$> (pure <$> operand)
    optOper ((Optional , a), op) = ((,a).(op:)) <$> (toList <$> optional operand)
    optOper ((None     , a), op) = ((,a).(op:)) <$> pure []

--------------------------------------------------------------------------------

-- isTok :: (Functor f, Eq (f ())) => f a -> f a -> Bool
-- isTok = on (==) (fmap (const ()))

node :: LdP d t (Cofree (Diagram c d t) DgExt)
node = ann <$> currentPos <*> (Node <$> node')

node' :: LdP d t [Cofree (Diagram c d t) DgExt]
node'
--     = branch
--         (isTok Cross)
--         [ (goRight, hline') --currentPosM>>=traceShowM>>
--         , (goDown , vline')
--         ]
    = cross
    *> ((++) <$> (toList <$> (optional $ keepOrigin $ goRight'' *> hline'))
             <*> (toList <$>
                ((keepOrigin (const Nothing <$> (goDown'' *> end2)))
                    <|> (optional $ id $ goDown'' *> vline'))))

--FIXME with 'node' may end only left rail, vline stemming from node must lead to another node
vline' :: LdP d t (Cofree (Diagram c d t) DgExt)
vline' = many vline *> (end2 <|> node)
--TODO for non-std ladders - check if crossing label and terminate at first `Node` returning `Sink`

end2 :: LdP d t (Cofree (Diagram c d t) DgExt)
end2 = end *> ((:< End) <$> colUnder <$> lastPos)

eol2 :: LdP d t (Cofree (Diagram c d t) DgExt)
eol2 = eol *> ((:< Sink) <$> colRight <$> lastPos)

gap2 :: LdP d t (Cofree (Diagram c d t) DgExt)
gap2 = gap *> ((:< Sink) <$> colRight <$> lastPos)

hline' :: LdP d t (Cofree (Diagram c d t) DgExt)
hline'
    = some hline2
    *> (device <|> node <|> jump <|> gap2 <|> eol2)

-- "-||`EOL`" is not allowed
hline2 :: LdP d t ()
hline2 = do
    vl <- hline
--FIXME don't do this, parse (skip) vlines
    when (vl > 0) do
        (ln, (co, _)) <- currentPos
        setPos (ln, (co + vl, ()))--TODO TEST move to same location is noop


device :: LdP d t (Cofree (Diagram c d t) DgExt)
device = do
    p        <- currentPos
    usr      <- getState
    (ops, f) <-
                withOperands do
                    dev <- coil' <|> contact'
                    case ctxMkDev usr dev of
                        Left  _             -> lift $ Left here
                        Right (flag, mkDev) -> return (flag, mkDev)
    dev'     <- case f ops of
                    Left  _ -> lift $ Left here
                    Right d ->  return d
    (p :<) <$> (Device dev' <$> hline')
