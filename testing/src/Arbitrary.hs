module Arbitrary where

import           Test.QuickCheck
import           Test.QuickCheck.Gen (oneof)

data Pair a b
    = Pair a b
    deriving (Eq, Show)

idGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
idGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = idGen

idGenInt :: Gen (Pair Int String)
idGenInt = idGen

-- Sum types
data Sum a b
    = First a
    | Second b
    deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual
