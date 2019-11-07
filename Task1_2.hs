module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = sum (take 10 terms)
    where terms = map (taylor_sin angle) [0,1..]
          angle = (fmod (x + pi) (2 * pi)) - pi
          fmod a b = a - (fromIntegral $ truncate (a / b)) * b
          taylor_sin y n = ((-1) ** n) * (y ** k) / (product [1..k])
              where k = 2 * n + 1
       
-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = todo

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x 1 = x
pow x y | even y = pow (x * x) (div y 2)
        | odd  y = x * pow (x * x) (div y 2)
                
-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | x < 4     = x > 1 
          | otherwise = null [k | k <- [2..(isqrt x)], (mod x k) == 0]
    where isqrt n = test n
              where test x | x * x > n = test(x - 1)
                           | otherwise = x
                
type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = 0.5 * abs (sumOfProducts xs (shift ys) - sumOfProducts (shift xs) ys) 
    where sumOfProducts a b = sum $ (zipWith (*)) a b 
          shift (h:t) = t ++ [h]
          xs = map fst points
          ys = map snd points
          
-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
