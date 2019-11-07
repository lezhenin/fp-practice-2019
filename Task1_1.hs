module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Op = Add
          | Substract
          | Multiply
          deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ op :: Op, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет

(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|+|) l r                             = BinaryTerm Add l r

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant (l - r)
(|-|) l r                             = BinaryTerm Substract l r

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant (l * r)
(|*|) l r                             = BinaryTerm Multiply l r

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement constant@(IntConstant _) = constant
replaceVar varName replacement varibale@(Variable name) | varName == name = replacement
                                                        | otherwise = varibale 
replaceVar varName replacement (BinaryTerm op lhv rhv)  = BinaryTerm op newLhv newRhv
    where newLhv = replaceVar varName replacement lhv
          newRhv = replaceVar varName replacement rhv

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate const@(IntConstant _)          = const
evaluate (BinaryTerm Add lhv rhv)       = (evaluate lhv) |+| (evaluate rhv)
evaluate (BinaryTerm Substract lhv rhv) = (evaluate lhv) |-| (evaluate rhv)
evaluate (BinaryTerm Multiply lhv rhv)  = (evaluate lhv) |*| (evaluate rhv)
evaluate _                              = error "Non constant term"                                                                         

