module Task5_2 where

import Todo(todo)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

instance Show a => Show (Zipper a) where
    show (Zipper [] []) = "Zipper([][])"
    show (Zipper l r) = "Zipper([" ++ (showL l) ++ "][" ++ (showR r) ++ "])"
        where showL [] = ""
              showR [] = ""
              showL [e] = show e
              showR [e] = show e
              showL (lh:lt) = (showL lt) ++ ", " ++ (show lh)
              showR (rh:rt) = (show rh) ++ ", " ++ (showR rt)

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concat :: Zipper a -> Zipper a -> Zipper a
concat left right = todo

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index what into = todo

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to into = (removeRigthMany from go( goEnd removeLeftMany ))

removeRigthMany 0 z = z
removeRigthMany n z = removeRigthMany (n - 1) (removeRigt z)

removeLeftMany 0 z = z
removeLeftMany n z = removeLeftMany (n - 1) (removeLeft z)

goStart z@(Zipper l []) = z
goStart z = goStart $ goLeft $ z

goEnd z@(Zipper [] r) = z
goEnd z = goEnd $ goRight $ z