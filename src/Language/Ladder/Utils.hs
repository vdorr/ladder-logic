{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Language.Ladder.Utils where

import Data.List
import Control.Applicative

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

pickFirst :: (a -> Bool) -> [a] -> (Maybe a, [a])
pickFirst p s
    = case break p s of
        (a, b:bs) -> (Just b , a ++ bs)
        _         -> (Nothing, s)

--------------------------------------------------------------------------------

istopo :: (a -> a -> Bool) -> [a] -> Bool
istopo dep (x : xs) = all (\y -> not $ dep x y) xs && istopo dep xs
istopo _ [] = True

istopoM :: (a -> a -> Bool) -> [a] -> Maybe a
istopoM dep (x : xs)
--     = case filter (dep x) xs of
--                             [] -> istopoM dep xs
--                             offender : _ -> Just offender
    = fst (pickFirst (dep x) xs) <|> istopoM dep xs
istopoM _ [] = Nothing

--i think i don't need to check for cycles here
isSpatialOrTopo :: (a -> a -> Bool) -> (a -> a -> Ordering) -> [a] -> Maybe a
isSpatialOrTopo dep spa = go
    where
    go (x : xs : xss)
        | spa x xs == LT || any (flip dep x) (xs:xss) = go (xs : xss)
        | otherwise = Just x
    go _ = Nothing

iscycle :: (a -> a -> Bool) -> (a -> a -> Bool) -> a -> [a] -> Bool
iscycle eq dep x = go x
    where
    go a as = case depend of
                   [] -> False
                   d | any (dep x) depend -> True --flip dep?
                   _ -> any (flip go indep) depend
        where
        (depend, indep) = partition (flip dep a) as

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
