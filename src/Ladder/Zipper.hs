
module Ladder.Zipper where

--------------------------------------------------------------------------------

data Zp a = Zp [a] [a]
    deriving (Show, Functor, Eq) -- , Foldable)

zpFromList :: [a] -> Zp a
zpFromList = Zp []

zpToList :: Zp a -> [a]
zpToList (Zp l r) = reverse l ++ r

zpLength :: Zp a -> Int
zpLength (Zp l r) = length l + length r

zpNull :: Zp a -> Bool
-- zpNull = (<=0) . zpLength
zpNull (Zp [] []) = True
zpNull _          = False

-- |Bring something into focus (cursed)
focus :: Zp a -> Zp a
focus (Zp (x:xs) []) = Zp xs [x]
focus zp             = zp

tip :: Zp a -> Maybe a
tip (Zp _ (x:_)) = Just x
tip _            =  Nothing

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

pattern ZpR' x <- Zp _ (x : _)
pattern ZpR l f r = Zp l (f : r)
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
moveTo move test zp@(ZpR l foc r) -- = undefined
    | test foc  = pure zp
    | otherwise = move zp >>= moveTo move test
moveTo _ _ _ = Nothing

move2 :: (a -> Ordering) -> Zp a -> Maybe (Zp a)
move2 f zp@(Zp _ (x : xs))
    | LT == f x   = moveTo stepLeft  ((==EQ).f) zp
    | otherwise   = moveTo stepRight ((==EQ).f) zp
move2 _ _ = Nothing

--------------------------------------------------------------------------------
