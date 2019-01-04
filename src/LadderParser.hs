{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, FlexibleContexts #-}
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

data Symbol
	= Source { location :: Pos, succs :: [Symbol] }
	| Sink { location :: Pos }
	| Device { location :: Pos, body :: String, options :: [String]
		, output :: Pos, succs :: [Symbol] }
	| Jump { location :: Pos, target :: String }
	| Label { location :: Pos, labelName :: String, nextRung :: Symbol }
--  	| Sink Pos
	| Node { location :: Pos, succs :: [Symbol] } --output is implied
	deriving Show

--------------------------------------------------------------------------------

type LDParser = Parser (Pos, [(Pos, Symbol)])

take3 :: LDParser Symbol
take3 = do
	(begin, _) <- getLocs
	setSource begin
	setDir (Pos (0, 1))
	char '|'
	Source begin <$> ((:[]) <$> vlink)

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
	Label begin lbl <$> vlink

jump :: LDParser Symbol
jump = do
	(begin, _src) <- getLocs
	void $ string ">>"
	lbl <- while isAlpha
	return $ Jump begin lbl --[src]

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
	Device begin (toUpper <$> devType) [n] end
		<$> ( (:[]) <$> hlink)--FIXME

node :: LDParser Symbol
node = peek >>= \case
		Value visited '+'
			| visited -> getLocs >>= \(loc, _)
				-> return $ Node loc []
			| otherwise
				-> justEatDontMove --XXX i guess 'eat' would work jsut as good, as pos is forced
					--XXX only it has to be moved after 'getLocs'
				>> getLocs >>= \(loc@(Pos(x, y)), _)
					-> Node loc <$> collect
						[ setLocs (Pos (x+1, y)) loc >> ( (:[]) <$> hlink)--FIXME
						, setLocs (Pos (x, y+1)) loc >> ( (:[]) <$> vlink)--FIXME
						]
		_ -> getLocs >>= \(loc, _) -> return $ Sink loc

collect :: (Monad m, Alternative m, Foldable t, Monoid a) => t (m a) -> m a
collect = foldM (\m f -> (flip mappend m <$> f) <|> pure m) mempty

hlink :: LDParser Symbol
hlink
	= setDir (Pos (1, 0))
	*> (char '-' *> hlink
		<|> jump
		<|> device
		<|> node)

vlink :: LDParser Symbol
vlink = setDir (Pos (0, 1))
	*> (char '|' *> vlink
		<|> lookahead (== '-') *> skip *> vlink
		<|> label
		<|> node)

--------------------------------------------------------------------------------

-- main = print 1
