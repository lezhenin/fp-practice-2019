module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)

import Prelude hiding (lookup, minimum, maximum)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
               | Leaf {key :: Integer, value :: v}
               | Node {key :: Integer, value :: v, lhs :: TreeMap v, rhs :: TreeMap v}
               deriving(Show,Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _           = False
contains (Leaf tk _) k         = k == tk
contains (Node tk _ lhs rhs) k | k == tk = True
                               | k <  tk = contains lhs k
                               | k >  tk = contains rhs k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> Maybe v
lookup k EmptyTree                        = Nothing
lookup k (Leaf tk tv)         | k == tk   = Just tv
                              | otherwise = Nothing
lookup k (Node tk tv lhs rhs) | k == tk   = Just tv
                              | k <  tk   = lookup k lhs
                              | k >  tk   = lookup k rhs

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree                      = Leaf  k  v
insert (k, v) (Leaf tk tv)         | k == tk = Leaf  k  v
                                   | k <  tk = Node tk tv (Leaf k v) EmptyTree 
                                   | k >  tk = Node tk tv EmptyTree (Leaf k v)
insert (k, v) (Node tk tv lhs rhs) | k == tk = Node  k  v lhs rhs
                                   | k <  tk = Node tk tv (insert (k, v) lhs) rhs
                                   | k >  tk = Node tk tv lhs (insert (k, v) rhs)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i EmptyTree                                         = EmptyTree
remove i t@(Leaf tk _)                         | i == tk   = EmptyTree
                                               | otherwise = t
remove i t@(Node tk tv (Leaf lk lv) EmptyTree) | i == lk   = Leaf tk tv
                                               | i == tk   = Leaf lk lv
                                               | otherwise = t
remove i t@(Node tk tv EmptyTree (Leaf lk lv)) | i == lk   = Leaf tk tv
                                               | i == tk   = Leaf lk lv
                                               | otherwise = t
remove i (Node tk tv lhs rhs)                  | i <  tk   = Node tk tv (remove i lhs) rhs
                                               | i >  tk   = Node tk tv lhs (remove i rhs)
                                               | i == tk   = Node mk mv lhs (remove mk rhs)
                                                                 where (mk, mv) = minimum rhs
                                                                       minimum (Leaf tk tv)             = (tk, tv)
                                                                       minimum (Node tk tv EmptyTree _) = (tk, tv)
                                                                       minimum (Node _ _ lhv _)         = minimum lhv

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> Maybe (Integer, v)
nearestLE i t = aux i t Nothing
    where aux i EmptyTree v                      = v
          aux i (Leaf tk tv) v         | i >= tk = Just (tk, tv)
                                       | i <  tk = v
          aux i (Node tk tv lhs rhs) v | i == tk = Just (tk, tv)
                                       | i >  tk = aux i rhs (Just (tk, tv))
                                       | i <  tk = aux i lhs v

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList []    = EmptyTree
treeFromList (h:t) = insert h (treeFromList t)

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree            = []
listFromTree (Leaf tk tv)         = (tk, tv) : []
listFromTree (Node tk tv lhs rhs) = (listFromTree lhs) ++ [(tk, tv)] ++ (listFromTree rhs)

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> Maybe (Integer, v)
kMean i t = listLookup i lst
                where lst = listFromTree t
                      listLookup _ []     = Nothing
                      listLookup 1 (x:_)  = Just x
                      listLookup n (_:xs) = listLookup (n - 1) xs
