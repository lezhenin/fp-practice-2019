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

integer :: Parser Integer
integer = read <$> do 
    integerPart <- (many1 digit)
    notFollowedBy (char '.')
    return integerPart
    
float :: Parser Double
float = read <$> do 
    integerPart <- (many1 digit)
    char '.'
    fractionalPart <- (many1 digit)
    return (integerPart ++ "." ++ fractionalPart)

atom :: (Fractional b) => Parser b
atom =  try (do { result <- float; return (fromRational $ toRational $ result) })
    <|> try (do { result <- integer; return (fromRational $ toRational $ result) })
    <|> try (do { char '('; result <- addition; char ')'; return result })

mulOperator :: (Fractional b) => Parser (b -> b -> b)
mulOperator =  do { char '*'; return (*) } 
           <|> do { char '/'; return (/) }

addOperator :: (Fractional b) => Parser (b -> b -> b)
addOperator =  do { char '+'; return (+) }
           <|> do { char '-'; return (-) }
           
negateOperator :: (Fractional b) => Parser (b -> b)
negateOperator = do { char '-'; return negate }

-- todo factorial

-- factorialOperator :: (Num b, Ord b) => Parser (b -> b)
-- factorialOperator = do{ char '!'; return factorial }
--     where factorial 0 = 1
--           factorial n | n > 0 = n * (factorial (n - 1))
--                       | otherwise = error "Factorial is undefined for negative numbers"

-- factorial :: (Num b, Ord b) => Parser b
-- factorial = do 
--     operand <- atom
--     optionalOperator <- option id factorialOperator
--     return $ optionalOperator $ operand 
    
multiplication :: (Fractional b) => Parser b
multiplication = chainl1 atom mulOperator

negation :: (Fractional b) => Parser b
negation = do 
    optionalOperator <- option id negateOperator
    operand <- multiplication
    return $ optionalOperator $ operand 
      
addition :: (Fractional b) => Parser b
addition = chainl1 negation addOperator

