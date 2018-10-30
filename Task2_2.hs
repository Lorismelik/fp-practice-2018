module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f b lst = case lst of
        [] -> b
        (h:t) -> foldl f (f b h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b lst =
    case lst of
        [] -> b
        (h:t) -> f h (foldr f b t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Just (a, b') -> a : unfoldr f b'
    Nothing -> []

lst = [4, 5, 3, 8, 1]

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\a acc -> (f a) : acc) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\ item lst -> case item of 
    Just item -> item:lst
    Nothing -> lst) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal x = zipWith (!!) x [0..]

matrix = [[1, 2, 3],
          [4, 5, 6],
          [7, 8, 9]]

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\a acc -> if f a then acc else a : acc) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x lst = foldr (||) False (map (\item -> item == x) lst)

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step   | to < from = error ":("
                       | to - from < step = [from]
                       | otherwise = unfoldr (\ x -> if x + step > to then Nothing else Just(x, x + step)) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append lst1 lst2 = foldr (:) lst1 lst2 

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\ x -> if null x  then Nothing else Just(take (fromIntegral n) x , drop (fromIntegral n) x)) lst
 
