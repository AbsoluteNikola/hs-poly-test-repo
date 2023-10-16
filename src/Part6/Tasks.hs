{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map
import Data.Bool (bool)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  zero :: Int -> Int -> mx
  eye ::  Int {-^ size -} -> mx
-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  zero _ _ = 0
  eye _ = 1

instance Matrix [[Int]] where
  zero w h = replicate h (replicate w 0)
  eye n =
    [ [ bool 0 1 (i == j) | j <- [0..n-1]]
    | i <- [0..n-1]
    ]


instance Matrix (SparseMatrix Int) where
  zero n m = SparseMatrix n m mempty
  eye n = SparseMatrix n n $ fromList [((i, i), 1) | i <- [0..n-1]]

-- Реализуйте следующие функции
-- Единичная матрица
-- eye :: Matrix m => Int -> m
-- eye w = fol
-- Матрица, заполненная нулями
-- zero :: Matrix m => Int -> Int -> m
-- zero w h = notImplementedYet
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
