module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции

Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf ['0'..'9']

number :: Parser Integer
number = read <$> many1 digit

applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t

div_ :: Parser (Integer -> Integer -> Integer)
div_ = do
    char '/'
    return div

star :: Parser (Integer -> Integer -> Integer)
star = do
    char '*'
    return (*)

plus :: Parser (Integer -> Integer -> Integer)
plus = do
    char '+'
    return (+)

minus :: Parser (Integer -> Integer -> Integer)
minus = do
    char '-'
    return (-)

multiplication :: Parser Integer
multiplication = do
    spaces
    lhv <- atom
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- star <|> div_
                spaces
                rhv <- atom
                spaces
                return (`f` rhv)

addition :: Parser Integer
addition = do
    spaces
    lhv <- multiplication
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- plus <|> minus
                spaces
                rhv <- multiplication
                spaces
                return (`f` rhv)

atom :: Parser Integer
atom = number <|> do
    char '('
    res <- addition
    char ')'
    return res
