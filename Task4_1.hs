module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- import Control.Applicative

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

-- Класс Functor описывает параметризованные типы,
-- данные внутри которых могут быть преобразованы.
instance Functor FunMonad where
    -- В данном случае к выходу функции fa необходимо
    -- преминить заданное преобразование f.
    fmap f (FunMonad fa) = FunMonad (f . fa)

-- Класс Applicative описывает параметризованные типы,
-- для которых определены операции для
--     помещения значения в контекст типа,
--     организации последовательности вычислений.
instance Applicative FunMonad where
    -- При помещении значения в контекст создается функция,
    -- которая возвращает данное значение независимо от аргемента.
    pure x = FunMonad (\_ -> x)
    -- Для применения функции в контексте ff к значению в контексте fa
    -- создается функция, в которой функция достается из контекста
    -- (ff s), значение достается из контекста (fa s) и функция
    -- применяется к значению ((ff s) $ (fa s)).
    (FunMonad ff) <*> (FunMonad fa) = FunMonad (\s -> (ff s) $ (fa s))

-- Класс Monad описывает параметризованные типы,
-- для которых определены операции для организации
-- последовательности действий, которая может быть описана
-- с помощью do синтаксиса.
instance Monad FunMonad where
    -- Для применения к значению в контексте fa функции f, которая
    -- возвращает значение в контексте, создается функция, в которой
    -- значение достается из контекста (fa $ s), к нему применяется
    -- заданная функция (f $ fa $ s). Далее из полученной монады
    -- достается функция (fun (f $ fa $ s)) и к ней применяется аргумент s.
    (FunMonad fa) >>= f = FunMonad (\s -> (fun (f $ fa $ s)) s)