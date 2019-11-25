{-# LANGUAGE TypeFamilies #-}

module Language.Ladder.Zipper where

-- import Data.Foldable
import GHC.Exts

--------------------------------------------------------------------------------

data Zp a = Zp ![a] ![a]
    deriving (Show, Functor, Eq) -- , Foldable)

instance Foldable Zp where
    foldMap f (Zp l r) = foldMap f $ reverse l ++ r
    null (Zp [] []) = True
    null _          = False
    length (Zp l r) = length l + length r

instance IsList (Zp a) where 
    type Item (Zp a) = a
    fromList = Zp []
    toList (Zp l r) = reverse l ++ r

zpFilter :: (a -> Bool) -> Zp a -> Zp a
zpFilter p (Zp l r) = Zp (filter p l) (filter p r)

-- |Bring something into focus (cursed)
focus :: Zp a -> Zp a
focus (Zp (x:xs) []) = Zp xs [x]
focus zp             = zp

tip :: Zp a -> Maybe a
tip (Zp _ (x:_)) = Just x
tip _            = Nothing

pop :: Zp a -> Maybe (a, Zp a)
pop (Zp l (x:r)) = Just (x, Zp l r)
pop _            = Nothing

push :: Zp a -> a -> Zp a
push (Zp l r) x = Zp l (x : r)

zpLookup :: Eq k => k -> Zp (k, v) -> Zp (k, v)
zpLookup needle (Zp l r@(c@(k, _) : rs))
    | needle == k   = Zp l r
    | otherwise     = zpLookup needle (Zp (c : l) rs)
zpLookup _ haystack = haystack

-- zpInsert :: a -> Zp a -> Zp a
-- zpInsert x (Zp l r) = Zp l (x : r)
-- 
-- zpInsertAfter :: a -> Zp a -> Zp a
-- zpInsertAfter x (Zp l (r : rs)) = Zp (r : l) (x : rs)
-- zpInsertAfter x (Zp l []) = Zp l [x]
-- 
-- -- |Append item and focus on it
-- zpAppend :: a -> Zp a -> Zp a
-- zpAppend x (Zp l r) = Zp (reverse r ++ l) [x]

-- pattern ZpR' x <- Zp _ (x : _)
pattern ZpR :: [a] -> a -> [a] -> Zp a
pattern ZpR l f r = Zp l (f : r)

pattern ZpL :: [a] -> a -> [a] -> Zp a
pattern ZpL l f r = Zp (f : l) r

stepLeft :: Zp a -> Maybe (Zp a)
stepLeft (ZpL l foc r) = Just (ZpR l foc r)
stepLeft _             = Nothing

stepRight :: Zp a -> Maybe (Zp a)
stepRight (ZpR l foc r) = Just (ZpL l foc r)
stepRight _             = Nothing

-- |Move to first element where predicate holds or fail
moveTo
    :: (Zp a -> Maybe (Zp a)) -- ^move function
    -> (a -> Bool) -- ^predicate
    -> Zp a
    -> Maybe (Zp a)
moveTo move test zp@(ZpR _ foc _) -- = undefined
    | test foc  = pure zp
    | otherwise = move zp >>= moveTo move test
moveTo _ _ _ = Nothing

move2 :: (a -> Ordering) -> Zp a -> Maybe (Zp a)
move2 f zp@(Zp _ (x : _xs))
    | LT == f x   = moveTo stepLeft  ((==EQ).f) zp
    | otherwise   = moveTo stepRight ((==EQ).f) zp
move2 _ _ = Nothing

--------------------------------------------------------------------------------
