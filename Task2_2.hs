module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x0 [] = x0
foldl f x0 (x:xs) = foldl f (f x0 x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x0 [] = x0
foldr f x0 (x:xs) = f x (foldr f x0 xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x0 = case f x0 of
        Just (a, b) -> a : unfoldr f b
        Nothing -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f lst = foldr (\x acc -> f x : acc) [] lst 

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of 
                    Just m -> m : acc
                    Nothing -> acc) []  

-- Диагональ матрицы
diagonal :: [[a]] -> [a] 
diagonal mat = snd $ foldr (\x (ind, acc) -> (ind - 1, x !! ind : acc)) (length mat - 1, []) mat  

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = foldr (\x acc -> case f x of
                        True -> x : acc
                        False -> acc) [] lst

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem a lst = foldl (\acc x -> if x == a then True else acc) False lst

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\x -> if x < to then Just (x, x + step) else Nothing) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append lst1 lst2 = foldr (:) lst2 lst1

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\x -> case x of 
                        [] -> Nothing
                        _ -> Just(take (fromIntegral n) x, drop (fromIntegral n) x)) lst


