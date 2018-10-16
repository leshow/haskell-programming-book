module Main where

import           Data.Monoid                    ( (<>) )

main :: IO ()
main = print "Hello"

-- Phantom types for added safety:
data Len = Km | Ml deriving (Show)

newtype Distance (a :: Len) = Distance Double deriving (Show, Eq, Ord)

kmToMl :: Distance 'Km -> Distance 'Ml
kmToMl (Distance km) = Distance $ 0.62 * km

mlToKm :: Distance 'Ml -> Distance 'Km
mlToKm (Distance ml) = Distance $ ml * 1.61

--
newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

(.*) :: (b -> c) -> (a -> b) -> a -> c
(.*) f g = \x -> f (g x)

--
data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
    foldMap _ Nil         = mempty
    foldMap f (Cons a as) = f a <> foldMap f as

    foldr _ b Nil         = b
    foldr f b (Cons a as) = f a (foldr f b as)

instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as

