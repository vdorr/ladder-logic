{-# LANGUAGE CPP, OverloadedStrings, TupleSections, TypeSynonymInstances, FlexibleInstances #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Control.Monad

import Preprocess

data Zp a = Zp [a] [a]

-- instance Functor Zp where
-- 	fmap f (Zp l r) = Zp (fmap f l) (fmap f r)

type Dg a = Zp (Int, Zp ((Int, Int), a))

up, down, left, right :: Dg a -> Maybe (Dg a)

up = undefined
down = undefined
left = undefined
right = undefined

peek :: Dg a -> Maybe a
eat :: Dg a -> Maybe (a, Dg a)

peek (Zp _ ((_, Zp _ ((_, x) : _)) : _)) = Just x
peek _ = Nothing

-- eat = undefined
eat (Zp u ((ln, Zp l ((_, x) : rs)) : ds)) = Just (x, Zp u ((ln, Zp l rs) : ds))
eat _ = Nothing

xxx
	:: [(Int, [((Int, Int), Tok)])]
	-> Dg Tok
-- xxx = fmap ( (Zp []) . ?? )
xxx = undefined

main = do
	[file] <- getArgs
	src <- TIO.readFile file
	case stripPos <$> preproc4 src of
		Left err -> TIO.putStrLn err
		Right x -> do
-- 			print $ stripPos x
			forM_ x $ \(l,c) -> do
				print l
				print c
