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
-- 
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
istopo _   []       = True

istopoM :: (a -> a -> Bool) -> [a] -> Maybe a
istopoM dep (x : xs) = fst (pickFirst (dep x) xs) <|> istopoM dep xs
istopoM _   []       = Nothing

-- isSpatialOrTopo :: (a -> a -> Bool) -> (a -> a -> Ordering) -> [a] -> Maybe a
-- isSpatialOrTopo dep spa g = go g
--     where
--     go (x : xs : xss)
--         | spa x xs == LT || any (flip dep x) (xs:xss) = go (xs : xss)
--         | otherwise = Just x
--     go _ = Nothing
isSpatialOrTopo :: (a -> a -> Bool) -> (a -> a -> Ordering) -> [a] -> Maybe (a, a)
isSpatialOrTopo dep spa g = (,) <$> istopoM dep g <*> isSorted sources
    where
    isSorted (x:xs:xss)
        | spa x xs == LT = isSorted (xs:xss)
        | otherwise      = Just x
    isSorted _          = Nothing

--TODO i should check if this fires in hedgehog
--is it true that this list is spatially sorted?
    sources = filter noPreds g
    noPreds v = all (\w -> spa v w /= EQ && not (dep v w)) g


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
