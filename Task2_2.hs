module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f d []     = d
foldl f d (x:xs) = foldl f (f d x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f d []     = d
foldr f d (x:xs) = f x (foldr f d xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f d = case (f d) of Just (k, l) -> k : unfoldr f l
                            Nothing     -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f l = unfoldr m l
    where m []     = Nothing
          m (x:xs) = Just (f x, xs)

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product l = foldl (*) 1 l

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes l = foldr f [] l
    where f (Just a) b  = a : b
          f Nothing b   = b

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal m = unfoldr f m
    where f []          = Nothing
          f l@((x:_):_) = Just (x, map cut (cut l))
              where cut []     = []
                    cut (x:xs) = xs

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f l = foldr g [] l
    where g a b | f a       = b
                | otherwise = a : b

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e l = foldl (\x -> \y -> x || (y == e)) False l

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from
    where f from | from < to = Just (from, from + step)
                 | otherwise = Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b = foldr (:) b a

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr f lst
    where f []  = Nothing
          f lst = Just (split lst n)
              where split [] _     = ([], [])
                    split lst 0    = ([], lst)
                    split (x:xs) n = (x:h, t)
                        where (h, t) = split xs (n - 1)

