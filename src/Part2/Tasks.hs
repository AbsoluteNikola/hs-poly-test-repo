module Part2.Tasks where

import Util(notImplementedYet)
import Debug.Trace (traceShowId)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement = \case
  c@IntConstant{} -> c
  Variable curName -> if curName == varName
    then replacement
    else Variable curName
  BinaryTerm{..} -> BinaryTerm
    { op = op
    , lhv = replaceVar varName replacement lhv
    , rhv = replaceVar varName replacement rhv
    }

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate = \case
  c@IntConstant{} -> c
  v@Variable{} -> v
  BinaryTerm{..} -> case (evaluate lhv, evaluate rhv) of
    (IntConstant x, IntConstant y) -> IntConstant $ case op of
      Plus -> x + y
      Minus -> x - y
      Times -> x * y
    (lhv', rhv') -> BinaryTerm op lhv' rhv'
