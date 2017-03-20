module Ch25 where

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

-- Compose :: (* -> *) -> (* -> *) -> * -> *
-- (.) :: (b -> c) -> (a -> b) -> a -> c

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
