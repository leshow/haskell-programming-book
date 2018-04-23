{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Data.Monoid    ((<>))
import           Prelude        hiding (init, min, zipWith)
import           Data.IORef
import           Control.Monad  (forM_, guard)

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

head :: List ('Succ n) a -> a
head (Cons x _) = x

tail :: List ('Succ n) a -> List n a
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
    Map f (x ': xs) = f x ': Map f xs

type family If (c :: Bool) (t :: *) (f :: *)
type instance If 'True t f = t
type instance If 'False t f = f

main :: IO ()
main = print "stuff"

testIORef :: IO ()
testIORef = do 
    ref <- newIORef (1 :: Int) 
    forM_ [1..10] $ \x ->
        modifyIORef ref (+x)
    readIORef ref >>= print

fizzbuzz :: Int -> String
fizzbuzz n = 
    concat (fizz <> num <> buzz)
    where
        fizz, buzz, num :: [String]
        fizz = "fizz" <$ guard (n `mod` 3 == 0)
        buzz = "buzz" <$ guard (n `mod` 5 == 0)
        num = show n <$ guard (null fizz && null buzz)

data Obj = forall e. Show e => Obj e

instance Show Obj where
    showsPrec p (Obj a) = showsPrec p a

pack :: Show a => a -> Obj
pack = Obj

test :: IO ()
test = print $ map f [pack (3 :: Int), pack "hello", pack 'c']
    where
        f :: Obj -> String
        f (Obj a) = show a

-- soln1
steps :: Int -> Int
steps n
    | n == 0    = 1
    | n == 1    = 1
    | n == 2    = 2
    | otherwise = steps (n-3) + steps (n-2) + steps (n-1)

--soln2
steps_ :: [Integer]
steps_ = 1 : 1 : 2 : zipWith3 sumOf steps_ (Prelude.tail steps_) (Prelude.tail (Prelude.tail steps_))

-- variadic functions (can take n arguments)
-- sumOf 1 2 3 4 5 :: Integer
-- 15
class SumRes r where 
    sumOf :: Integer -> r

instance SumRes Integer where
    sumOf = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
    sumOf x = sumOf . (x +) . toInteger

-- natural transformation
infixr 0 ~>
type f ~> g = forall x. f x -> g x

maybeHead :: [] ~> Maybe
maybeHead [] = Nothing
maybeHead (x:_) = Just x
