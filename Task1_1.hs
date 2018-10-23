module Task1_1 where

import Todo(todo)

data Operation = Plus | Minus | Multiply
            deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, oper :: Operation } -- бинарная операция
            deriving(Show,Eq)


-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Minus
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Multiply

infixl 5 |+|
infixl 5 |-|
infixl 6 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case (expression) of
          Variable var | var == varName -> replacement
          BinaryTerm lhv rhv oper -> BinaryTerm (replaceVar varName replacement lhv) (replaceVar varName replacement rhv) oper
          _ -> expression
-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm l r oper) = 
    let lhv = evaluate l in
    let rhv = evaluate r in
    case oper of
    Plus -> case (lhv, rhv) of
        (IntConstant lhv, IntConstant rhv) -> IntConstant (lhv + rhv)
        (IntConstant 0, rhv) -> rhv
        (lhv, IntConstant 0) -> lhv
        otherwise -> BinaryTerm lhv rhv Plus
    Minus -> case (lhv, rhv) of
        (IntConstant lhv, IntConstant rhv) -> IntConstant (lhv - rhv)
        (lhv, IntConstant 0) -> lhv
        otherwise -> BinaryTerm lhv rhv Minus
    Multiply -> case (lhv, rhv) of
        (IntConstant lhv, IntConstant rhv) -> IntConstant (lhv * rhv)
        (IntConstant 0, rhv) -> IntConstant 0
        (IntConstant 1, rhv) -> rhv
        (lhv, IntConstant 0) -> IntConstant 0
        (lhv, IntConstant 1) -> lhv
        otherwise -> BinaryTerm lhv rhv Multiply
    otherwise -> BinaryTerm l r oper
evaluate expression = expression