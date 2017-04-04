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
    first f (Deux a b) = Deux (f a) b
    second f (Deux a b) = Deux a (f b)

data Const a b = Const a

instance Bifunctor Const where
    bimap f g (Const a) = Const (f a)
    first f (Const a) = Const (f a)
    second f (Const a) = Const a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)
    first f (Drei a b c) = Drei a (f b) c
    second f (Drei a b c) = Drei a b (f c)


data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f g (SuperDrei a b) = SuperDrei a (f b)
    first f (SuperDrei a b) = SuperDrei a (f b)
    second f (SuperDrei a b) = SuperDrei a b

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap f g (SemiDrei a) = SemiDrei a
    first f (SemiDrei a) = SemiDrei a
    second f (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadriceps a b c d

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadriceps a b c d) = Quadriceps a b (f c) (g d)
    first f (Quadriceps a b c d) = Quadriceps a b (f c) d
    second f (Quadriceps a b c d) = Quadriceps a b c (f d)

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
    bimap f g (Left' a)  = Left' (f a)
    bimap f g (Right' b) = Right' (g b)

    first f (Left' a)  = Left' (f a)
    first f (Right' b) = Right' b

    second f (Left' a)  = Left' a
    second f (Right' b) = Right' (f b)
    second f (Right' b) = Right' (f b)


