module Task3_3 where

import Data.Semigroup

newtype PSet a = PSet{ contains :: (a -> Bool) }


-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


-- 1. Сумма множеств (A ∪ B), моноидом для этой операции будет являться (a -> False)
-- [ элемент может не находится в одном из множеств ]

newtype PSetAdd a = PSetAdd { containsAdd :: (a -> Bool) }

instance Monoid (PSetAdd a) where
    mempty = PSetAdd (\x -> False)

instance Semigroup (PSetAdd a) where
    (<>) (PSetAdd a) (PSetAdd b) = PSetAdd(\x -> a x || b x)

-- 2. Произведение множеств (A ∩ B), моноидом для этой операции будет являться (a -> True)
-- [ элемент может входить в множество А, но при этом не находится в множестве B ]

newtype PSetOr a = PSetOr { containsOr :: (a -> Bool) }

instance Monoid (PSetOr a) where
    mempty = PSetOr (\x -> True)

instance Semigroup (PSetOr a) where
    (<>) (PSetOr a) (PSetOr b) = PSetOr(\x -> a x && b x)

-- 3. Симметричная разность множеств (A Δ B), моноидом для этой операции будет являться (a -> False) 
-- [ элемент может не находится в одном из множеств, значит он не на пересечении и находится в другом множестве ]

newtype PSetSymDiff a = PSetSymDiff { containsSymDiff :: (a -> Bool) }

instance Monoid (PSetSymDiff a) where
    mempty = PSetSymDiff (\x -> False)

instance Semigroup (PSetSymDiff a) where
    (<>) (PSetSymDiff a) (PSetSymDiff b) = PSetSymDiff(\x -> (a x || b x) && not (a x && b x))