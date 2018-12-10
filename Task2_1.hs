module Task2_1 where

import Todo(todo)

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа

data TreeMap v = EmptyTreeMap
                | Node {key :: Integer, 
                        leftChild :: TreeMap v, 
                        value :: v, 
                        rightChild ::TreeMap v}
                deriving (Eq, Show)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTreeMap

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTreeMap k = False
contains (Node key l _ r ) k | key == k = True
                             | key > k = contains l k
                             | key < k = contains r k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k EmptyTreeMap = error "Key not found"
lookup k (Node key l v r) | key == k = v
                          | key > k = lookup k l
                          | key < k = lookup k r

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTreeMap = Node k EmptyTreeMap v EmptyTreeMap
insert (k, v) (Node key l value r) | key > k = Node key ( insert (k,v) l ) value r
                                   | key < k = Node key l value ( insert (k,v) r )

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i (Node key l v r) | key == i = chooseCase (Node key l v r) 
                          | key > i = Node key ( remove i l ) v r 
                          | key < i = Node key l v ( remove i r )

chooseCase :: TreeMap v -> TreeMap v
chooseCase (Node k l v r) =  case (k, l, r) of
                                    (_, EmptyTreeMap, EmptyTreeMap) -> EmptyTreeMap
                                    (_, EmptyTreeMap, _) -> r 
                                    (_, _, EmptyTreeMap) -> l
                                    (k, l, r) -> moveNodes k l r

moveNodes :: Integer -> TreeMap v -> TreeMap v -> TreeMap v
moveNodes k (Node k1 l1 v1 r1) (Node k2 l2 v2 r2) | k > k2 = Node k2 (insert (k1,v1) l2) v2 r2
                                                  | otherwise = Node k1 l1 v1 (insert (k2,v2) r1)

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i (Node k l v r) | k == i = (k,v) 
                           | otherwise = case (l, r) of
                                    (EmptyTreeMap, EmptyTreeMap) -> (k,v)
                                    (_, EmptyTreeMap) -> if k > i then nearestLE i l else (k,v)
                                    (EmptyTreeMap, _) -> if k < i then nearestLE i r else (k,v)
                                    _ -> if k > i then  nearestLE i l else nearestLE i r


-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTreeMap lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTreeMap = []
listFromTree (Node k l v r) = listFromTree l ++ [(k, v)] ++ listFromTree r

treeMapSize :: TreeMap v -> Integer
treeMapSize (Node _ l _ r) = treeMapSize l + 1 + treeMapSize r

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ EmptyTreeMap = error "Tree is empty"
kMean i (Node k l v r) | treeMapSize l == i = (k,v)
                       | treeMapSize l > i  = kMean i l
                       | otherwise = kMean (i - (treeMapSize l) - 1) r