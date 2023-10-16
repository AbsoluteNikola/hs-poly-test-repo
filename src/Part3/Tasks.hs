module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (group, sortOn, sort, groupBy)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f x = map f [x..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq = flip (-) (fromEnum '0') . fst . head . sortOn ((0 -) . snd)
  . map (\x -> (fromEnum (head x), length x))
  . group . sort . concat . map show

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = go []
  where
    go used [] = []
    go used (x:xs) = if x `elem` used
      then go used xs
      else x : go (x : used) xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = map (\xs -> (fst . head $ xs, map snd xs))
  . groupBy (\(fst -> x) (fst -> y) -> x == y)
  $ zip (map f l) l
