module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

instance Foldable ReverseList where
    foldr f x0 RNil = x0
    foldr f x0 (RCons xs x) = f x (foldr f x0 xs)  

rlistToList :: ReverseList a -> [a]
rlistToList = foldl (\acc x -> x : acc) []

listToRList :: [a] -> ReverseList a
listToRList = foldl (\acc x -> RCons acc x) RNil

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show (RCons xs x) = show x ++ ":" ++ show xs

instance (Eq a) => Eq (ReverseList a) where
    (==) a b = case (a, b) of
        (RNil, RNil) -> True
        (RCons xs x, RCons ys y) -> x == y && xs == ys
        _ -> False

instance (Ord a) => Ord (ReverseList a) where
    compare a b = case (a, b) of
        (RNil, RNil) -> EQ
        (RNil, _) -> LT
        (_, RNil) -> GT
        (f, s) -> compare' f s
        where
            compare' (RCons xs x) (RCons ys y) | x > y = GT
                                               | x < y = LT
                                               | otherwise = compare xs ys

instance Monoid (ReverseList a) where
    mempty = RNil

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons xs x) = RCons (fmap f xs) (f x)