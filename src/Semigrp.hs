module Semigrp where

import           Data.List.NonEmpty as N
import           Data.Semigroup
import           Test.QuickCheck
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
    a <> b = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a)  where
    Identity a <> Identity b = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two x y) = Two (a <> x) (b <> y)

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

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj False, BoolConj True]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False
    BoolDisj _ <> BoolDisj _ = BoolDisj True

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


-- tests
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

runSemiTests = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
