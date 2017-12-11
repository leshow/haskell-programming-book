{-# LANGUAGE TypeOperators #-}

import Data.Traversable
import Data.Foldable
import Data.Monoid ((<>))

data Triple a = Triple a a a

instance Functor Triple where
    fmap f (Triple a b c) = Triple (f a) (f b) (f c)

main :: IO ()
main = print "stuff"

instance Applicative Triple where
    pure a = Triple a a a
    (Triple f g h) <*> (Triple a b c) = Triple (f a) (g b) (h c)

instance Foldable Triple where
    foldMap f (Triple a b c) = f a <> f b <> f c

instance Traversable Triple where
    traverse f (Triple a b c) = Triple <$> f a <*> f b <*> f c

newtype (:.) f g x = Comp {comp :: f (g x)}

instance (Functor f, Functor g) => Functor (f :. g) where
    fmap f (Comp fga) = Comp $ (fmap f) <$> fga

instance (Applicative f, Applicative g) => Applicative (f :. g) where
    pure a = Comp (pure (pure a))
    (Comp fgf) <*> (Comp fg) = Comp $ (fmap (<*>) fgf) <*> fg

instance (Foldable f, Foldable g) => Foldable (f :. g) where
    foldMap f (Comp g) = foldMap (foldMap f) g

instance (Traversable f, Traversable g) => Traversable (f :. g) where
    traverse f (Comp g) = pure Comp <*> traverse (traverse f) g
