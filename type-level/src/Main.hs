{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Data.Monoid ((<>))
import           Prelude     hiding (init, min, replicate, zipWith)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort xs = merge (msort l) (msort r)
    where
        half = length xs `div` 2
        l = take half xs
        r = drop half xs

-- type-level dependently lengthed lists

data Nat = Zero | Succ Nat

data List (n :: Nat) (a :: *) where
    Nil :: List 'Zero a
    Cons :: a -> List n a -> List ('Succ n) a

deriving instance Eq a => Eq (List n a)

instance Show a => Show (List n a) where
    show Nil         = "Nil"
    show (Cons a as) = "Cons" <> show a <> " (" <> show as <> ") "

head :: List (Succ n) a -> a
head (Cons x _) = x

tail :: List (Succ n) a -> List n a
tail (Cons _ xs) = xs

-- add
type family Add (n :: Nat) (m :: Nat) where
    Add 'Zero m = m
    Add ('Succ n) m = 'Succ (Add n m)

infixl 6 :+

type (:+) (n :: Nat) (m :: Nat) = Add n m

append :: List n a -> List m a -> List (Add n m) a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)


infixl 7 :*
-- multiply
type family Mul (n :: Nat) (m :: Nat) :: Nat where
    Mul 'Zero m = 'Zero
    Mul n 'Zero = 'Zero
    Mul ('Succ n) m = Add (Mul n m) m

type (:*) (n :: Nat) (m :: Nat) = Mul n m

-- fromList :: Nat -> [a] -> Maybe (List n a)
-- fromList Zero _          = Just Nil
-- fromList (Succ n) (x:xs) = Cons x <$> fromList n xs
-- fromList _ _             = Nothing

toList :: List n a -> [a]
toList = lfoldr (:) []

lfoldr :: (a -> b -> b) -> b -> List n a -> b
lfoldr _ b Nil         = b
lfoldr f b (Cons x xs) = f x (lfoldr f b xs)

lmap :: (a -> b) -> List n a -> List n b
lmap _ Nil         = Nil
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

zipWithSame :: (a -> b -> c) -> List n a -> List n b -> List n c
zipWithSame _ Nil Nil                 = Nil
zipWithSame f (Cons a as) (Cons b bs) = Cons (f a b) (zipWithSame f as bs)

type family Min (n :: Nat) (m :: Nat) :: Nat where
    Min 'Zero 'Zero = 'Zero
    Min ('Succ n) 'Zero = 'Zero
    Min 'Zero ('Succ m) = 'Zero
    Min ('Succ n) ('Succ m) = 'Succ (Min n m)

min :: Nat -> Nat -> Nat
min Zero Zero         = Zero
min Zero (Succ _)     = Zero
min (Succ _) Zero     = Zero
min (Succ n) (Succ m) = Succ (min n m)

zipWith :: (a -> b -> c) -> List n a -> List m b -> List (Min n m) c
zipWith _ Nil Nil                 = Nil
zipWith _ (Cons _ _) Nil          = Nil
zipWith _ Nil (Cons _ _)          = Nil
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) $ zipWith f as bs

init :: List ('Succ n) a -> List n a
init (Cons x xs) = case xs of
    Nil        -> Nil
    (Cons _ _) -> Cons x (init xs)

type family Map (f :: * -> *) (xs :: [*]) where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': (Map f xs)
