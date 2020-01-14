module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов

index :: DList a -> Int -> a
index (DCons _ value _) 0 = value
index (DCons _ _ right) i = index right (i - 1)

insertAt :: DList a -> Int -> a -> DList a
insertAt DNil 0 newValue = list
    where list = (DCons DNil newValue DNil)
insertAt (DCons left value DNil) 1 newValue = list
    where list = update (DCons left value (DCons list newValue DNil))
insertAt (DCons left value right) 0 newValue = list
    where list = update (DCons left newValue (DCons list value right))
insertAt (DCons _ _ right) index newValue = list
    where list = left (insertAt right (index - 1) newValue)

removeAt :: DList a -> Int -> DList a
removeAt (DCons left preccValue (DCons _ _ DNil)) 1 = list
    where list = update (DCons left preccValue DNil)
removeAt (DCons left _ (DCons _ succValue right)) 0 = list
    where list = update (DCons left succValue right)
removeAt (DCons _ _ right) index = list
    where list = left (removeAt right (index - 1))

-- Функции для обновления списка

update :: DList a -> DList a
update DNil = DNil
update (DCons DNil value DNil) = list
    where list = DCons DNil value DNil
update (DCons DNil value (DCons _ succValue right)) = list
    where list = DCons DNil value (updateToRight (DCons list succValue right))
update (DCons (DCons left precValue _) value DNil) = list
    where list = DCons (updateToLeft (DCons left precValue list)) value DNil
update (DCons (DCons left precValue _) value (DCons _ succValue right)) = list
    where list = DCons (updateToLeft (DCons left precValue list)) value (updateToRight (DCons list succValue right))

updateToRight :: DList a -> DList a
updateToRight DNil = DNil
updateToRight list@(DCons left value DNil) = list
updateToRight (DCons left value (DCons _ succValue right)) = list
    where list = DCons left value (updateToRight (DCons list succValue right))

updateToLeft :: DList a -> DList a
updateToLeft DNil = DNil
updateToLeft list@(DCons DNil value right) = list
updateToLeft (DCons (DCons left precValue _) value right) = list
    where list = DCons (updateToLeft (DCons left precValue list)) value right

