{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards
  , FlexibleContexts, DeriveFunctor, QuantifiedConstraints
  , DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-incomplete-patterns
                     -fwarn-unused-binds
                     -fwarn-unused-imports #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module LadderParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State
import Control.Monad.Except

import DiagramParser

--------------------------------------------------------------------------------

-- data Symbol
-- 	= Source { location :: Pos, succ :: Symbol }
-- 	| Sink { location :: Pos }
-- 	| Device { location :: Pos, body :: String, options :: [String]
-- 		, output :: Pos, succ :: Symbol }
-- 	| Jump { location :: Pos, target :: String }
-- 	| Label { location :: Pos, labelName :: String, nextRung :: Symbol }
-- 	| Node { location :: Pos, succs :: [Symbol] } --output is implied
-- 	| Node' { location :: Pos } --already visited Node
-- 	deriving Show

--------------------------------------------------------------------------------

data Symbol_ a
	= Source a
	| Sink
	| Device String [String] a
	| Jump String
	| Label String a
	| Node [a]
-- 	| Node' --already visited Node
	deriving (Show, Functor, Foldable, Traversable)

data Cofree f a = a :< f (Cofree f a)
	deriving (Functor, Foldable, Traversable)
--unFix :: Cofree f a -> (a, f (Cofree f a))
--unFix (a :< x) = (a, x)
--cata :: Functor f => ((w, f a) -> a) -> Cofree f w -> a
--cata alg = alg . fmap (fmap (cata alg)) . unFix
unFix :: Cofree f a -> f (Cofree f a)
unFix (_ :< f) = f
cata :: Functor f => (f a -> a) -> Cofree f w -> a
cata alg = alg . fmap (cata alg) . unFix

unFix' :: Cofree f a -> (a, f (Cofree f a))
unFix' (a :< f) = (a, f)
cata' :: Functor f => ((w, f a) -> a) -> Cofree f w -> a
cata' alg = alg . fmap (fmap (cata' alg)) . unFix'

type Symbol = Cofree Symbol_ Pos

cof a f = (a :<) . f

instance (forall t. Show t => Show (f t), Show a) => Show (Cofree f a) where
	show (a :< as) = show a ++ " :< (" ++ show as ++ ")"

--------------------------------------------------------------------------------

type LDParser = Parser (Pos, [(Pos, Symbol)])

take3 :: LDParser Symbol
take3 = do
	(begin, _) <- getLocs
	setSource begin
	setDir (Pos (0, 1))
	char '|'
	cof begin Source <$> vlink

--------------------------------------------------------------------------------

getLocs :: LDParser (Pos, Pos)
getLocs = get >>= \PS{..} -> return (pos, fst user)

setLocs :: Pos -> Pos -> LDParser ()
setLocs pos' src' = modify $ \ps@PS{..} -> ps { pos=pos', user=(src', snd user)}

isDevStart :: Char -> Bool
isDevStart c = Prelude.elem c "([" -- ++ ")]" --XXX for now, only l-r direction

isVariable :: Char -> Bool
isVariable c = isAlphaNum c || Prelude.elem c "%."

--------------------------------------------------------------------------------

--XXX avoid
add :: Pos -> Symbol -> LDParser ()
add begin x = modify $ \st -> let (p, u) = user st in st { user = (p, (begin, x) : u) }

--XXX avoid
setSource :: Pos -> LDParser ()
setSource p = modify $ \st -> st { user = (p, snd $ user st) }

--------------------------------------------------------------------------------

label :: LDParser Symbol
label = do
	(begin@(Pos(x, y)), _) <- getLocs
	lbl <- withDir (Pos(1, 0))
		$ while isAlpha <* char ':'
	setPos (Pos(x, y+1))
	cof begin (Label lbl) <$> vlink

jump :: LDParser Symbol
jump = do
	(begin, _src) <- getLocs
	void $ string ">>"
	lbl <- while isAlpha
	return $ begin :< Jump lbl

varName
	:: Pos --pos of first char of device body
	-> Int -- +-1
	-> LDParser String
varName (Pos(x, y)) dy = do
	setPos (Pos(x, y + dy)) --part of variable name to the left
	s0 <- withDir (Pos(-1, 0)) $ while isVariable
	setPos (Pos(x+1, y + dy)) --part of variable to the left
	s1 <- withDir (Pos(1, 0)) $ while isVariable
	return $ Prelude.reverse s0 ++ s1

device :: LDParser Symbol --TODO [Symbol]
device = do
	(begin, _) <- getLocs
	devType <- string "[ ]"
		<|> string "[/]"
		<|> string "( )"
		<|> string "(s)" <|> string "(S)"
		<|> string "(r)" <|> string "(R)"
		<|> throwError here
	(end, _) <- getLocs
	n <- varName begin (-1)
	setPos end --restore position to where parsing should continue
	setSource end
	cof begin (Device (toUpper <$> devType) [n]) <$> hlink

node :: LDParser Symbol
node = peek >>= \case
		Value visited '+'
			| visited -> getLocs >>= \(loc, _)
-- 				-> return $ loc :< Node' --XXX is this even allowed?
				-> return $ loc :< Node [] --XXX is this even allowed?
			| otherwise
				-> justEatDontMove --XXX i guess 'eat' would work jsut as good, as pos is forced
					--XXX only it has to be moved after 'getLocs'
				>> getLocs >>= \(loc@(Pos(x, y)), _)
					-> cof loc Node <$> collect
						[ setLocs (Pos (x+1, y)) loc >> ( (:[]) <$> hlink)--FIXME
						, setLocs (Pos (x, y+1)) loc >> ( (:[]) <$> vlink)--FIXME
						]
		_ -> getLocs >>= \(loc, _) -> return $ loc :< Sink

collect :: (Monad m, Alternative m, Foldable t, Monoid a) => t (m a) -> m a
collect = foldM (\m f -> (flip mappend m <$> f) <|> pure m) mempty

hlink :: LDParser Symbol
hlink
	= setDir (Pos (1, 0))
	*> (char '-' *> hlink
		<|> jump
		<|> device
		<|> node)

topDown = Pos (0, 1)

vlink :: LDParser Symbol
vlink = setDir topDown
	*> (char '|' *> vlink
		<|> lookahead (== '-') *> skip *> vlink --walking over horizontal link
		<|> label
		<|> node)
