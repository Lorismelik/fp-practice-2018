module Task1_1 where

import Todo(todo)

data Operator = Plus | Minus | Mult
                deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ operator :: Operator, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm Plus l r
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm Minus l r
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm Mult l r


infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = replace expression
    where
        replace (Variable variable) = 
            if variable == varName then replacement else expression
        replace (BinaryTerm operator lhv rhv) =  BinaryTerm operator (replace lhv) (replace rhv)
        replace _ = expression


-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
    BinaryTerm operator lhv rhv ->
        case (operator, evaluate(lhv), evaluate(rhv)) of 
            (Plus, IntConstant lhv, IntConstant rhv) -> IntConstant (lhv + rhv)
            (Minus, IntConstant lhv, IntConstant rhv) -> IntConstant (lhv - rhv)
            (Mult, IntConstant lhv, IntConstant rhv) -> IntConstant (lhv * rhv)
            (Plus, IntConstant 0, rhv) -> rhv
            (Mult, IntConstant 1, rhv) -> rhv
            (Mult, IntConstant 0, rhv) -> IntConstant 0
            (Plus, lhv, IntConstant 0) -> lhv
            (Minus, lhv, IntConstant 0) -> lhv
            (Mult, lhv, IntConstant 1) -> lhv
            (Mult, lhv, IntConstant 0) -> IntConstant 0
            _ -> BinaryTerm operator lhv rhv
    _ -> expression
