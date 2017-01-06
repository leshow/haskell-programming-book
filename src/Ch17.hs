module Ch17 where

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- class Functor f => Applicative f where
--      pure :: a -> f a
--      (<*>) :: f (a -> b) -> f a -> f b
-- every applicative must also have a functor instance
-- Control.Applicative also provides liftA liftA2 liftA3
-- liftA is fmap but with a applicative constraint instead of functor
-- liftA2 and A3 are fmap but can take more arguments
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- Applicatives are Monoidal Functors

-- also, you can define functor in terms of fmap, for example
-- fmap f x == pure f <*> x
-- and
-- fmap (+1) [1,2,3] == pure (+1) <*> [1,2,3]


-- ("Woo", (+1)) <*> ("Hoo!", 0) -- ("WooHoo!", 1)

-- notice the strings concatenated. this is due to Monoid instances for that value

import           Control.Applicative
import           Data.List           (elemIndex)
import           Data.Monoid         ((<>))

added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]


tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z
-- (,) <$> y <*> zpp

x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'
-- :t max' <$> x'
-- Maybe (Int -> Int)  aka. f (a -> b) -- the first arg to applicative's <*>
-- so we can take that and use <*> y' to pass in f a

xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 2 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')
-- sum <$> liftA2 (,) x'' y''


newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) fa = f <$> fa

newtype Constant a b
    = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure a = Constant { getConstant = mempty }
    -- following are both valid (I think)
    -- (<*>) (Constant a) (Constant b) = Constant { getConstant = a `mappend` b }
    fab <*> fa = Constant (getConstant fab <> getConstant fa)
