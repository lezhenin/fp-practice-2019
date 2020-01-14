module Task5_2 where

import Todo(todo)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

instance (Show a) => Show (Zipper a) where
    show (Zipper [] []) = "Zipper([][])"
    show (Zipper l r) = "Zipper([" ++ (showLeft l) ++ "][" ++ (showRight r) ++ "])"
        where showLeft [] = ""
              showLeft [e] = show e
              showLeft (lh:lt) = (showLeft lt) ++ ", " ++ (show lh)
              showRight [] = ""
              showRight [e] = show e
              showRight (rh:rt) = (show rh) ++ ", " ++ (showRight rt)
              
instance (Eq a) => Eq (Zipper a) where
    (Zipper ll lr) == (Zipper rl rr) = (reverse ll ++ lr) == (reverse rl ++ rr)

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
concat left@(Zipper ll lr) right = (Zipper ll (lr ++ rr))
    where (Zipper _ rr) = goStart right

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index what@(Zipper wl wr) into = (Zipper (wl ++ l) (wr ++ r)) 
    where (Zipper l r) = (goRightMany index (goStart into))

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to into = if new_pos > 0 
                         then if new_pos <= to
                               -- восстановление положения указателя 
                              then goRightMany new_pos (goStart (Zipper il []))
                              else (Zipper il [])
                         else goStart (Zipper il []) 
    where (Zipper il _) = goRightMany (to - from) (removeRightMany from (goStart into)) 
          new_pos = (position into) - from
          
-- Вспомогательные функции  
 
removeRightMany :: Int -> Zipper a -> Zipper a
removeRightMany 0 z = z
removeRightMany n z = removeRightMany (n - 1) (removeRight z)

removeLeftMany :: Int -> Zipper a -> Zipper a
removeLeftMany 0 z = z
removeLeftMany n z = removeLeftMany (n - 1) (removeLeft z)

goLeftMany :: Int -> Zipper a -> Zipper a
goLeftMany 0 z = z
goLeftMany n z = goLeftMany (n - 1) (goLeft z)

goRightMany :: Int -> Zipper a -> Zipper a
goRightMany 0 z = z
goRightMany n z = goRightMany (n - 1) (goRight z)

goStart :: Zipper a -> Zipper a
goStart z@(Zipper [] _) = z
goStart z = goStart $ goLeft $ z

goEnd :: Zipper a -> Zipper a
goEnd z@(Zipper _ []) = z
goEnd z = goEnd $ goRight $ z

position :: Zipper a -> Int
position (Zipper [] _) = 0
position z@(Zipper _ _) = 1 + (position $ removeLeft $ z) 

