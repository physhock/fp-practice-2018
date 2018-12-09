module Task1_1 where

import Todo(todo)

data BiOper = Add | Sub | Mult
            deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, biOper :: BiOper, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|-|) :: Term -> Term -> Term
(|*|) :: Term -> Term -> Term

infixl 6 |+|, |-|
infixl 7 |*|

(|-|) l r = BinaryTerm l Sub r
(|+|) l r = BinaryTerm l Add r 
(|*|) l r = BinaryTerm l Mult r  

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = expression
replaceVar varName replacement (Variable var) = if var == varName then replacement else Variable var
replaceVar varName replacement (BinaryTerm lhv op rhv) = BinaryTerm (replaceVar varName replacement lhv) op (replaceVar varName replacement rhv)


-- Посчитать значение выражения `Term`
-- если оно состоит только из константа
evaluate :: Term -> Term
evaluate expression = todo