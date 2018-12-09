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
replaceVar varName replacement expression = case expression of
    Variable var -> if var == varName then replacement else Variable var
    BinaryTerm lhv op rhv -> BinaryTerm (replaceVar varName replacement lhv) op (replaceVar varName replacement rhv)
    _ -> expression


-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
    BinaryTerm lhv op rhv -> case (l, op, r) of
        (l, Add, IntConstant 0) -> l
        (IntConstant 0, Add, r) -> r
        (IntConstant l, Add, IntConstant r) -> IntConstant $ r + l
        (IntConstant l, Sub, IntConstant r) -> IntConstant $ l -r
        (l, Sub, IntConstant 0) -> l
        (IntConstant l, Mult, IntConstant r) -> IntConstant $ l * r
        (l, Mult, IntConstant 0) -> IntConstant 0
        (IntConstant 0, Mult, r) -> IntConstant 0
        (l, Mult, IntConstant 1) -> l
        (IntConstant 1, Mult, r) -> r
        _ -> BinaryTerm l op r
        where
            l = evaluate lhv
            r = evaluate rhv
    _ -> expression