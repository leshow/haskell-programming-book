{-# LANGUAGE ExplicitForAll #-}

module Ch20 where

import           Data.Monoid

-- class Foldable (t :: * -> *) where
--     fold :: forall m. Monoid m => t m -> m
--     foldMap :: forall a m. Monoid m => (a -> m) -> t a -> m


-- should be clear from this definition whereas foldr takes some function and a starting value,
-- fold just takes a list of monoidaly things and applies mappend between them all
-- foldr (++) [] ["hello", "world"] == fold ["hello", "world"]


data Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

data Opt a = Nada | Yasa a

instance Foldable Opt where
    foldr _ z Nada     = z
    foldr f z (Yasa a) = f a z

    foldl _ z Nada     = z
    foldl f z (Yasa a) = f z a

    foldMap _ Nada     = mempty
    foldMap f (Yasa a) = f a
