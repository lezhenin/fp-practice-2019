module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

import Data.Ratio
import Control.Exception

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where

    show x = "WeirdPeanoNumber(" ++ (show $ toInteger $ x) ++ ")"


instance Num WeirdPeanoNumber where
    
    Zero + b                = b
    (Succ a) + b@(Succ _)   = Succ (a + b)
    (Pred a) + b@(Pred _)   = Pred (a + b) 
    (Succ a) + (Pred b)     = a + b
    a + b                   = b + a
    
    Zero * b                = Zero
    (Succ a) * b@(Succ _)   = b + (b * a)
    (Pred a) * b@(Pred _)   = b + (b * a)
    a@(Succ _) * b@(Pred _) = negate (a * (negate b))
    a * b                   = b * a
     
    negate Zero             = Zero
    negate (Succ a)         = Pred (negate a) 
    negate (Pred a)         = Succ (negate a)
    
    abs Zero                = Zero
    abs a@(Succ _)          = a
    abs a@(Pred _)          = negate a
    
    signum Zero             = Zero
    signum (Succ _)         = Succ Zero
    signum (Pred _)         = Pred Zero
    
    fromInteger i           | signum i ==  0 = Zero
                            | signum i ==  1 = Succ (fromInteger (i - 1))
                            | signum i == -1 = Pred (fromInteger (i + 1))


instance Eq WeirdPeanoNumber where

    a == b = case (a - b) of Zero      -> True
                             otherwise -> False 


instance Ord WeirdPeanoNumber where

    compare a b = case signum (a - b) of Zero        -> EQ
                                         (Succ Zero) -> GT
                                         (Pred Zero) -> LT

                                         
instance Real WeirdPeanoNumber where

    toRational x = (toInteger x) % 1
                      

instance Enum WeirdPeanoNumber where
    
    toEnum   = fromInteger . toInteger
    fromEnum = fromInteger . toInteger 
    
    
instance Integral WeirdPeanoNumber where
    
    quotRem a Zero = throw DivideByZero
    quotRem a b | b < 0     = (negate q, r) where (q, r) = quotRem a (negate b)
    quotRem a b | a < 0     = (negate q, negate r) where (q, r) = quotRem (negate a) b                           
    quotRem a b | otherwise = aux a b (Zero, a)
        where aux a b (q, r) | a >= b    = aux (a - b) b ((q + 1), (a - b))
                             | otherwise = (q, r) 
    
    toInteger Zero = 0
    toInteger (Succ a) = (toInteger a) + 1
    toInteger (Pred a) = (toInteger a) - 1
     
    
