{-# LANGUAGE ExplicitForAll #-}

module Ch20 where

import           Data.Foldable
import           Data.Monoid
import           Prelude       hiding (length, maximum, minimum, null, product,
                                sum)


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

-- Library Exercises

-- 1.
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . foldMap (\x -> Any $ x == a)

-- 4.
-- minimum :: (Foldable t, Ord a) => t a -> Maybe a
-- minimum = fmap getMin . getOption . foldMap (Option . Just . Min)
data Min a = Min { getMin :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mappend (Min Nothing) a = a
    mappend a (Min Nothing) = a
    mappend (Min a) (Min b) = Min $ min a b

chmin :: (Foldable t, Ord a) => t a -> Maybe a
chmin = getMin . foldMap (\x -> Min {getMin = Just x}) -- equivalent to Min $ Just x OR Min . Just

-- 5.

data Max a = Max { getMax :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mappend (Max Nothing) a = a
    mappend a (Max Nothing) = a
    mappend (Max a) (Max b) = Max $ max a b

chmax :: (Foldable t, Ord a) => t a -> Maybe a
chmax = getMax . foldMap (Max . Just)

-- 6
null :: (Foldable t) => t a -> Bool
null = getAll . foldMap (\_ -> All False)

-- 7
length :: Foldable t => t a -> Int
length = foldr (\_ acc -> acc + 1) 0

-- 8
toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- 9
myfold :: (Foldable t, Monoid m) => t m -> m
myfold = foldr (<>) mempty

-- 10
myFoldMap :: (Foldable t, Functor t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = fold . fmap f

otherFMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
otherFMap f = foldr (\x acc -> f x `mappend` acc) mempty

-- Ch 20 exercises

data Constant a b = Constant a

instance Foldable (Constant a) where
    foldMap f (Constant a) = mempty

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c
