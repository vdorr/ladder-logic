{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Language.Ladder.Utils where

--------------------------------------------------------------------------------

--FIXME i don't like this any more
data Cofree f a = a :< f (Cofree f a)
    deriving (Functor, Foldable, Traversable)

instance (Eq a, Eq (f (Cofree f a))) => Eq (Cofree f a) where
    a :< b == c :< d = a == c && b == d
-- 
instance (Functor f, forall t. Show t => Show (f t), Show a) => Show (Cofree f a) where
--     show (a :< as) = "(" ++ show a ++ " :< " ++ show as ++ ")"
    show (a :< n) = "(\n" ++ pad (show a) ++ " :< " ++ show (fmap (Nope . show) n)  ++ ")"
        where
        pad = take 12 . (++repeat ' ') --FIXME

newtype Nope = Nope String
instance Show Nope where
    show (Nope s) = s

--------------------------------------------------------------------------------

unFix' :: Cofree f a -> (a, f (Cofree f a))
unFix' (a :< f) = (a, f)

cata' :: Functor f => ((w, f a) -> a) -> Cofree f w -> a
cata' alg = alg . fmap (fmap (cata' alg)) . unFix'

--------------------------------------------------------------------------------

pickFirst :: (a -> Bool) -> [a] -> (Maybe a, [a])
pickFirst p s
    = case break p s of
        (a, b:bs) -> (Just b , a ++ bs)
        _         -> (Nothing, s)

--------------------------------------------------------------------------------

--TODO tests
-- stability - without dependencies order is unchanged
-- topology - either topo order is satisfied or it is cycle (or no dependency)
sttsort :: (a -> a -> Bool) -> [a] -> [a]
sttsort depOn = f
    where
    f (x : xs) = dep ++ [x] ++ f indep
        where
        (dep, indep) = g x xs
    f [] = []

    g x xs
        = case pickFirst (depOn x) xs of
            (Just x1, x1s) ->
                let
                    (dep1, indep1) = g x1 x1s
                    (dep, indep) = g x indep1
                in
                    (dep1++[x1]++dep , indep )
            (Nothing, xs1) -> ([], xs1)

--------------------------------------------------------------------------------
