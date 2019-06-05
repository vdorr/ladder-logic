{-# LANGUAGE FlexibleContexts, DeriveFunctor, QuantifiedConstraints
  , DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE UndecidableInstances #-}

module Language.Ladder.Utils where

--------------------------------------------------------------------------------

--FIXME i don't like this any more
data Cofree f a = a :< f (Cofree f a)
    deriving (Functor, Foldable, Traversable)

instance (Eq a, Eq (f (Cofree f a))) => Eq (Cofree f a) where
    a :< b == c :< d = a == c && b == d

--unFix :: Cofree f a -> (a, f (Cofree f a))
--unFix (a :< x) = (a, x)
--cata :: Functor f => ((w, f a) -> a) -> Cofree f w -> a
--cata alg = alg . fmap (fmap (cata alg)) . unFix
-- unFix :: Cofree f a -> f (Cofree f a)
-- unFix (_ :< f) = f
-- cata :: Functor f => (f a -> a) -> Cofree f w -> a
-- cata alg = alg . fmap (cata alg) . unFix
-- 
-- unFix' :: Cofree f a -> (a, f (Cofree f a))
-- unFix' (a :< f) = (a, f)
-- cata' :: Functor f => ((w, f a) -> a) -> Cofree f w -> a
-- cata' alg = alg . fmap (fmap (cata' alg)) . unFix'
-- 
-- type Symbol = Cofree (Symbol_ String) Pos
-- 
-- cof a f = (a :<) . f

instance (forall t. Show t => Show (f t), Show a) => Show (Cofree f a) where
    show (a :< as) = "(" ++ show a ++ " :< " ++ show as ++ ")"

--------------------------------------------------------------------------------

unFix' :: Cofree f a -> (a, f (Cofree f a))
unFix' (a :< f) = (a, f)

cata' :: Functor f => ((w, f a) -> a) -> Cofree f w -> a
cata' alg = alg . fmap (fmap (cata' alg)) . unFix'

--------------------------------------------------------------------------------
