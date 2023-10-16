module Part4.Tasks where

import Util(notImplementedYet)
import Debug.Trace (traceShowId)
import Control.Applicative

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
  reverse (reversed lst)
  where
    reversed REmpty = []
    reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = go REmpty
  where
    go acc [] = acc
    go acc (x:xs) = go (acc :< x) xs

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where

  show x = "[" <> go x <> "]"
    where
      go REmpty = ""
      go (REmpty :< x) = show x
      go (xs :< x) = go xs <> "," <> show x

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) (xs :< x) (ys :< y) = x == y && xs == ys
    (==) _ _ = False
    (/=) x y = not $ x == y

instance Semigroup (ReverseList a) where
  x <> REmpty = x
  xs <> (ys :< y) = (xs <> ys) :< y
instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap f REmpty = REmpty
  fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
  pure x = REmpty :< x
  liftA2 f REmpty _ = REmpty
  liftA2 f _ REmpty = REmpty
  liftA2 f (xs :< x) ys = liftA2 f xs ys <> fmap (f x) ys

instance Monad ReverseList where
  xss >>= f = case xss of
    REmpty -> REmpty
    xs :< x -> (xs >>= f) <> f x
