{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Ch21 where

import           Data.Monoid              ((<>))
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


type T1 = Identity
type T2 = Constant Int
type T3 = Optional
type T4 = List
type T5 = Three Int Int
type T6 = Three' Int
type T7 = S Maybe
type T8 = Tree

run = do
    quickBatch (traversable (undefined :: T1 (Int, Int, [Int])))
    quickBatch (traversable (undefined :: T2 (Int, Int, [Int])))
    quickBatch (traversable (undefined :: T3 (Int, Int, [Int])))
    quickBatch (traversable (undefined :: T4 (Int, Int, [Int])))
    quickBatch (traversable (undefined :: T5 (Int, Int, [Int])))
    quickBatch (traversable (undefined :: T6 (Int, Int, [Int])))
    quickBatch (traversable (undefined :: T7 (Int, Int, [Int])))

-- 1
newtype Identity a = Identity a
    deriving (Eq, Show, Ord)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    Identity f <*> a = f <$> a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

-- 2
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure a = Constant { getConstant = mempty }
    Constant a <*> Constant b = Constant { getConstant = a `mappend` b }

instance Foldable (Constant a) where
    foldMap f (Constant a) = mempty

instance Traversable (Constant a) where
    traverse f (Constant a) = pure $ Constant a

-- 3

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        elements [Nada, Yep a]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep $ f a

instance Applicative Optional where
    pure = Yep
    (Yep f) <*> fa = f <$> fa
    Nada <*> fa = Nada

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

-- 3

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(5, Cons <$> arbitrary <*> arbitrary), (1, return Nil)]

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a fa) = Cons (f a) (fmap f fa)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil a          = a
    mappend a Nil          = a
    mappend (Cons a la) lb = Cons a (la `mappend` lb)

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fa <*> ls = mappend (fmap f ls) (fa <*> ls)

instance Foldable List where
    foldMap _ Nil         = mempty
    foldMap f (Cons a ls) = f a `mappend` foldMap f ls
    -- just for fun
    foldr f b Nil         = b
    foldr f b (Cons a ls) = f a (foldr f b ls)
    -- little bit of practice
    foldl f b Nil         = b
    foldl f b (Cons a ls) = foldl f (f b a) ls
    
instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons a ls) = Cons <$> f a <*> traverse f ls

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    Three fa fb fc <*> Three a b c = Three (fa <> a) (fb <> b) (fc c)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

-- 5

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
    foldMap f (Three' a b b') = f b <> f b'

instance Traversable (Three' a) where
    traverse f (Three' a b b') = Three' a <$> f b <*> f b'

-- 6

data S n a = S (n a) a deriving (Eq, Show)


instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

instance Functor n => Functor (S n) where
    fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
    traverse f (S n a) = S <$> traverse f n <*> f a

-- 7

data Tree a
    = Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node ta a tb) = Node (fmap f ta) (f a) (fmap f tb)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node ta a tb) = foldMap f ta <> f a <> foldMap f tb

    foldr _ b Empty = b
    foldr f b (Leaf a) = f a b
    foldr f b (Node ta a tb) = foldr f (f a (foldr f b ta)) tb

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node ta a tb) = Node <$> traverse f ta <*> f a <*> traverse f tb
