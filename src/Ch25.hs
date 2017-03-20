{-# LANGUAGE InstanceSigs #-}

module Ch25 where

import           Control.Applicative (liftA2)

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

-- Compose :: (* -> *) -> (* -> *) -> * -> *
-- (.) :: (b -> c) -> (a -> b) -> a -> c

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ (pure . pure) a

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    Compose f <*> Compose a = Compose $ liftA2 (<*>) f a


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga



class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g
    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id
    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a

instance Bifunctor Const where
    bimap f g (Const a) = Const (f a)
