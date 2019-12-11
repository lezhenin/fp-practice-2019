module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)
import Data.Semigroup

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil        = []
rlistToList (RCons l e) = (rlistToList l) ++ [e]

listToRList :: [a] -> ReverseList a
listToRList []     = RNil
listToRList (x:xs) = (RCons RNil x) <> listToRList xs

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance Show a => Show (ReverseList a) where
    show RNil = "ReverseList([])"
    show l = (aux l) ++ "])"
        where aux (RCons RNil e) = "ReverseList([" ++ (show e)
              aux (RCons l e)  = (aux l) ++ "," ++ (show e)

instance Eq a => Eq (ReverseList a) where
    (==) RNil RNil                               = True
    (==) (RCons xl xe) (RCons yl ye) | xe == ye  = xl == yl
                                     | otherwise = False
    (==) xs ys                                   = False

instance Ord a => Ord (ReverseList a) where
    compare RNil RNil                   = EQ
    compare RNil _                      = LT
    compare _ RNil                      = GT
    compare (RCons xl xe) (RCons yl ye) = case compare xe ye of
                                            EQ    -> compare xl yl
                                            other -> other

instance Semigroup (ReverseList a) where
    (<>) b RNil           = b
    (<>) b (RCons l e)    = RCons (b <> l) e

instance Monoid (ReverseList a) where
    mempty  = RNil
    mappend = (<>)

instance Functor ReverseList where
    fmap _ RNil         = RNil
    fmap f (RCons l e)  = RCons (fmap f l) (f e)