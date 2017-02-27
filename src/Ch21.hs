{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Ch21 where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
-- Traversable is a superclass of Foldable that defines a type which
-- maps each element of a structure to an action, and evaluates actions from left
-- to right.

-- {-# MINIMAL traverse | sequenceA #-}
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse f = sequenceA . fmap f

-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA = traverse id
-- you can see from the fn signature it flips the structure of it's argument around t <=> f
-- fmap sum $ sequenceA [Just 1, Just 2, Just 3]
-- Just 6
-- sequenceA flipped: [Maybe Int] <=> Maybe [Int]
-- however if there's a Nothing in the list, the whole thing will return Nothing
-- fmap sum $ sequenceA [Just 1, Just 2, Nothing]
-- Nothing

data Either' a b = Left' a | Right' b deriving (Eq, Show, Ord)

instance Functor (Either' a) where
    fmap f (Left' a)  = Left' a
    fmap f (Right' b) = Right' $ f b

instance Applicative (Either' a) where
    pure = Right'
    Right' f <*> a = f <$> a
    Left' f <*> _ = Left' f

instance Foldable (Either' a) where
    foldMap _ (Left' _)  = mempty
    foldMap f (Right' y) = f y

    foldr _ z (Left' _)  = z
    foldr f z (Right' b) = f b z

instance Traversable (Either' a) where
    traverse _ (Left' fa)  = pure (Left' fa)
    traverse f (Right' fa) = Right' <$> f fa -- notice how the type flips here
    sequenceA (Right' fa) = Right' <$> fa
    sequenceA (Left' fa)  = pure (Left' fa)
-- traversable laws
-- 1. naturality
-- t . traverse f = traverse (t . f)
-- 2. identity
-- traverse Identity = Identity    is another way of saying traversable instance cant add or inject any structure/effects
-- 3. composition
-- traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f


type TI = []

run = do
    let trigger = undefined :: TI (Int, Int, [Int])
    quickBatch (traversable trigger)

newtype Identity a = Identity a
    deriving (Eq, Show, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    Identity f <*> a = f <$> a

-- instance Foldable Identity where
--     foldMap
--
-- instance Traversable Identity where
--     traverse f (Identity a) = Identity <$> f a
