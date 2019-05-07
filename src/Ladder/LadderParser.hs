{-# LANGUAGE FlexibleContexts, DeriveFunctor, QuantifiedConstraints
  , DeriveFoldable, DeriveTraversable #-}

{-# LANGUAGE UndecidableInstances #-}

module Ladder.LadderParser where

--------------------------------------------------------------------------------

data Diagram d v s a
    = Source a -- ^start of power rail
    | Sink           -- ^where wire connects to (implied) right rail
    | End            -- ^where vertical left rail ends at the bottom
--     | Stub -- maybe better 
    | Device d [v] a --
    | Jump   s
    | Node   [a]     --order matters here
--     | Label s --i dislike it, but would need it if vline can cross line with label
    deriving (Show, Functor, Eq)

--------------------------------------------------------------------------------

-- data Device s
--     = Contact s
--     | Coil    s
--     | Block
--     deriving (Show, Eq)

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
