module Part1.Tasks where

import Util(notImplementedYet)
import Data.Bool (bool)
import Debug.Trace (traceShowId)

unify :: (Floating a, RealFrac a) => a -> a
unify x = x - 2 * pi * (fromIntegral . round $ ( x / (2 * pi)))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin (unify -> x) = res
  where
    res = sum . filter (not . isNaN) $
      [(((-1) ** n) * (x ** (2 * n + 1))) / product [1..(2 * n + 1)]
      | n <- [0..200]]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos (unify -> x) = sum $ [(((-1) ** n) * (x ** (2 * n))) / product [1..(2 * n)] | n <- [0..200]]

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | year < 0 || month < 1 || month > 12 || day < 1 = False
  | month == 2 && isLeapYear = day <= 29
  | otherwise = day <= maxDay
  where
    maxDay = [31,28,31,30,31,30,31,31,30,31,30,31] !! fromIntegral (month - 1)
    isLeapYear = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow x n
  | n < 0 = 0  -- Error: Negative exponent
  | otherwise = x * myPow x (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | n <= 3 = True
  | otherwise = all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
  let (x, y) = unzip points
      n = length points
  in 0.5 * abs
    (sum [ x !! i * y !! (i + 1) - x !! (i + 1) * y !! i | i <- [0..n - 2] ]
       + last x * head y - last y * head x)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | a <= 0 || b <= 0 || c <= 0 = -1
  | a + b <= c || a + c <= b || b + c <= a = -1
  | a ** 2 + b ** 2 == c ** 2 || a ** 2 + c ** 2 == b ** 2 || b ** 2 + c ** 2 == a ** 2 = 2
  | a ** 2 + b ** 2 < c ** 2 || a ** 2 + c ** 2 < b ** 2 || b ** 2 + c ** 2 < a ** 2 = 0
  | otherwise = 1
