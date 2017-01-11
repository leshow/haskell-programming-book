module ZipList where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [ (5, Cons <$> arbitrary <*> arbitrary)
                        , (1, return Nil)
                        ]

instance Eq a => EqProp (List a) where (=-=) = eq

instance Functor List where
  fmap f Nil         = Nil
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)

append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Applicative List where
  pure a = Cons a Nil
  fs <*> xs = flatMap (<$> xs) fs

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons a as) = Cons a (take' (n-1) as)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (Cons x xs) (Cons x' xs') = Cons (f x x') (zipWith' f xs xs')
zipWith' _ _ _                       = Nil

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  (ZipList' l) <*> (ZipList' l') = ZipList' $ zipWith' ($) l l'

runTests :: IO ()
runTests = do
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  quickBatch $ functor (undefined :: List (String, Char, Integer))
  quickBatch $ applicative (undefined :: List (String, Char, Integer))
  quickBatch $ functor (undefined :: ZipList' (String, Char, Integer))
  quickBatch $ applicative (undefined :: ZipList' (String, Char, Integer))
