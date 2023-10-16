module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f start [] = start
myFoldl f start (x:xs) = myFoldl f (f start x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr  f start [] = start
myFoldr f start (x:xs) = f x (myFoldr f start xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x b -> f x : b) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr (\x b -> f x <> b) []

myConcat :: [[a]] -> [a]
myConcat = myConcatMap id

myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr f []
  where
    f x b = if p x
      then x : b
      else b

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p xs = (myFilter p xs, myFilter (not . p) xs)
