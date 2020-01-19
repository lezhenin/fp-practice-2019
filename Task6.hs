module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции

Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor


data Constant = IntConstant Int | FloatConstant Double
    deriving(Show)

(|+|) :: Constant -> Constant -> Constant
(|+|) (IntConstant a) (IntConstant b) = IntConstant (a + b)
(|+|) (FloatConstant a) (FloatConstant b) = FloatConstant (a + b)
(|+|) (FloatConstant a) (IntConstant b) = FloatConstant (a + (fromIntegral b))
(|+|) a b = b |+| a

(|-|) :: Constant -> Constant -> Constant
(|-|) (IntConstant a) (IntConstant b) = IntConstant (a - b)
(|-|) (FloatConstant a) (FloatConstant b) = FloatConstant (a - b)
(|-|) (FloatConstant a) (IntConstant b) = FloatConstant (a - (fromIntegral b))
(|-|) a b = b |-| a

(|*|) :: Constant -> Constant -> Constant
(|*|) (IntConstant a) (IntConstant b) = IntConstant (a * b)
(|*|) (FloatConstant a) (FloatConstant b) = FloatConstant (a * b)
(|*|) (FloatConstant a) (IntConstant b) = FloatConstant (a * (fromIntegral b))
(|*|) a b = b |*| a

(|/|) :: Constant -> Constant -> Constant
(|/|) (IntConstant a) (IntConstant b) = FloatConstant ((fromIntegral a) / (fromIntegral b))
(|/|) (FloatConstant a) (FloatConstant b) = FloatConstant (a / b)
(|/|) (FloatConstant a) (IntConstant b) = FloatConstant (a / (fromIntegral b))
(|/|) a b = b |/| a

negateConstant :: Constant -> Constant
negateConstant (IntConstant a) = IntConstant (-a)
negateConstant (FloatConstant a) = FloatConstant (-a)

factorialConstant :: Constant -> Constant
factorialConstant (FloatConstant n) = error ""
factorialConstant (IntConstant 0) = IntConstant 1
factorialConstant (IntConstant n) | n > 0 = IntConstant (product [1..n])
                                  | otherwise = error ""
                                                

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf ['0'..'9']

integer :: Parser Int
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
    
constant :: Parser Constant 
constant = try (do { result <- float; return $ FloatConstant $ result })
       <|> try (do { result <- integer; return $ IntConstant $ result })
       
parentheses :: Parser Constant    
parentheses = do { char '('; result <- addition; char ')'; return result }

atom :: Parser Constant
atom =  constant <|> parentheses

multiplicationOperator :: Parser (Constant -> Constant -> Constant)
multiplicationOperator =  do { char '*'; return (|*|) } 
                      <|> do { char '/'; return (|/|) }

additionOperator :: Parser (Constant -> Constant -> Constant)
additionOperator =  do { char '+'; return (|+|) }
                <|> do { char '-'; return (|-|) }
           
negationOperator :: Parser (Constant -> Constant)
negationOperator = do { char '-'; return negateConstant }

factorialOperator :: Parser (Constant -> Constant)
factorialOperator = do{ char '!'; return factorialConstant }

factorial :: Parser Constant
factorial = do 
    operand <- atom
    optionalOperator <- option id factorialOperator
    return $ optionalOperator $ operand 
    
multiplication :: Parser Constant
multiplication = chainl1 factorial multiplicationOperator

negation :: Parser Constant
negation = do 
    optionalOperator <- option id negationOperator
    operand <- multiplication
    return $ optionalOperator $ operand 
      
addition :: Parser Constant
addition = chainl1 negation additionOperator

