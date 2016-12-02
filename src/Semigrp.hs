module Semigrp where

import           Data.List.NonEmpty as N
import           Data.Semigroup
import           Test.QuickCheck    hiding (Failure, Success)
-- Semigroup is Monoid minus the identity laws.
-- class Semigroup a where
-- (<>) :: a -> a -> a  -- it is in GHC 8 and up

-- in the case of a list, this is a non empty list. since [] is identity for [a]

xs = 1 :| [2,3]
ys = 4 :| [5,6]
zs = xs <> ys
-- 1 :| [2,3,4,5,6]

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a)  where
    Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity a) (Identity b) = Identity (a `mappend` b)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two x y) = Two (a <> x) (b <> y)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend (Two a b) (Two x y) = Two (a `mappend` x) (b `mappend` y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeAssoc = Three String String String
                -> Three String String String
                -> Three String String String
                -> Bool

-- 5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four x y z v) = Four (a <> x) (b <> y) (c <> z) (d <> v)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type FourAssoc = Four String String String String
                -> Four String String String String
                -> Four String String String String
                -> Bool

-- 6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    BoolConj _ <> BoolConj _ = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj False, BoolConj True]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False
    BoolDisj _ <> BoolDisj _ = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj False, BoolDisj True]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


-- 8

data Or a b
    = Fst a
    | Snd b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    Snd a <> _ = Snd a
    _ <> Snd b = Snd b
    _ <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]

type OrAssoc = Or String String
            -> Or String String
            -> Or String String
            -> Bool

-- 9

newtype Combine a b = Combine { unCombine :: a -> b }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine $ \m -> f m <> g m

instance (Monoid a, Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ \m -> mempty
    mappend (Combine f) (Combine g) = Combine $ \m -> f m `mappend` g m

-- 10

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup a => Semigroup (Comp a) where
    Comp a <> Comp b = Comp $ \m -> a m <> b m

instance Monoid a => Monoid (Comp a) where
    mempty = Comp id
    mappend (Comp f) (Comp g) = Comp $ f . g

-- 11

data Validation a b
    = Failure a
    | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Success a <> Success b = Success a
    Failure a <> Success b = Success b
    Success a <> Failure b = Success a
    Failure a <> Failure b = Failure (a <> b) -- this is allowed since 'a' is a semigroup instance
    -- and Failure values are always 'a'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Failure a, Success b]

type ValidAssoc = Validation String String
                -> Validation String String
                -> Validation String String
                -> Bool

-- 12

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    AccumulateRight (Success a) <> AccumulateRight (Success b) = AccumulateRight (Success (a <> b))
    AccumulateRight (Success a) <> AccumulateRight (Failure b) = AccumulateRight (Success a)
    AccumulateRight (Failure a) <> AccumulateRight (Success b) = AccumulateRight (Success b)
    AccumulateRight (Failure a) <> AccumulateRight (Failure b) = AccumulateRight (Failure a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [AccumulateRight (Failure a), AccumulateRight (Success b)]

type AccRAssoc = AccumulateRight String String
                -> AccumulateRight String String
                -> AccumulateRight String String
                -> Bool

-- 13

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    AccumulateBoth a <> AccumulateBoth b = AccumulateBoth (a <> b)
    -- if we know a and b are Semigroup then we can destructure and delegate to their instances

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [AccumulateBoth (Failure a), AccumulateBoth (Success b)]

type AccBAssoc = AccumulateBoth String String
                -> AccumulateBoth String String
                -> AccumulateBoth String String
                -> Bool


-- Monoid 8

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend (Mem f) (Mem g) = Mem $ \s -> (fst (f s) `mappend` fst (g s), snd (g (snd $ f s)))

-- tests
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidAssoc :: (Semigroup m, Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Semigroup m, Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Semigroup m, Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

runSemiTests = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck (semigroupAssoc :: ValidAssoc)
    quickCheck (semigroupAssoc :: AccRAssoc)
    quickCheck (semigroupAssoc :: AccBAssoc)

runMonoidTests = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
