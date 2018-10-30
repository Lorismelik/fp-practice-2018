module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Nil 
                | Node Integer v (TreeMap v) (TreeMap v) Integer
                deriving(Show, Eq)



-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Nil

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains t k = case(t) of 
    Nil -> False
    Node key _ _ _ _| key == k -> True
    Node key _ l _ _| key > k -> contains l k
    Node key _ _ r _| key < k -> contains r k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k t = case(t) of 
    Nil -> error "Key not found"
    Node key val _ _ _| key == k -> val
    Node key _ l _ _| key > k -> lookup k l 
    Node key _ _ r _| key < k -> lookup k r 

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) t = case(t) of 
    Nil -> Node k v Nil Nil 1
    Node key _ _ _ _| key == k -> error "Duplicate key"
    Node key val l r s| key > k -> Node key val (insert (k, v) l) r (s + 1)  
    Node key val l r s| key < k -> Node key val l (insert (k, v) r) (s + 1) 

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i t = case(t) of 
    Nil -> error "Key not found"


-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = case(t) of 
    Nil -> error "Could not find"
    Node key val l r _ | key == i -> (key, val)
                       | key > i -> nearestLE i l
                       | key < i -> case(r) of
                          Node k v _ _ _| k == i -> (k, v)
                          Node k v _ _ _| k < i -> nearestLE i r
                          otherwise -> (key, val)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Nil lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = case(t) of
    Nil -> []
    Node k v l r _ -> listFromTree l ++ [(k, v)] ++ listFromTree r



-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = case (t) of
    Nil -> error ":("
    Node k v l r s | (sizeOf l) == i -> (k, v)
                   | (sizeOf l) > i ->  kMean i l
                   | (sizeOf l) < i ->  kMean (i - (sizeOf l) - 1) r
    


sizeOf :: TreeMap v -> Integer
sizeOf (Nil) = 0
sizeOf (Node _ _ _ _ s) = s

nums = [(8, 0),(6, 0),(4, 0),(1, 0),(7, 0),(3, 0),(5, 0)]

tree = treeFromList nums