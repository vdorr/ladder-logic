{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, RecordWildCards, BangPatterns,
  FlexibleContexts, RecursiveDo, TupleSections, GADTSyntax, DeriveFunctor #-}

module Compile where

--------------------------------------------------------------------------------

data Pg lbl m a where
	Do :: m a -> Pg lbl m a
	Go :: lbl -> Pg lbl m a
	Br :: lbl -> m Bool -> Pg lbl m a


weave1 :: forall lbl m a. (Eq lbl, Monad m) => [(lbl, Pg lbl m a)] -> m a -> m a
weave1 [] end = end
weave1 src end = snd $ head program
	where

	program = f src

	f [(lbl, x)] = [(lbl, h x end)]
	f ((lbl, x) : xs) = let
		xs'@((_, xx) : _) = f xs
		in (lbl,h x xx) : xs'

	h (Do a) next = a >> next
	h (Go lbl) _ = getLbl lbl
	h (Br lbl cond) next = let
		dst = getLbl lbl
		in cond >>= \flag -> if flag then dst else next

	getLbl lbl = maybe (error "label not found") id $ lookup lbl program

--------------------------------------------------------------------------------
