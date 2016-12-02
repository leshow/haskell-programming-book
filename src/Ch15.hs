module Ch15 where

import           Control.Monad
import           Data.Monoid     ((<>))
import           Test.QuickCheck

data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty     = Nada

    mappend Nada Nada         = Nada
    mappend Nada (Only a)     = Only a
    mappend (Only a) Nada     = Only a
    mappend (Only a) (Only b) = Only (a <> b)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
    mconcat [e, "! he said",
            adv, " as he jumped into his car",
            noun, " and he drove off with his ",
            adj, " wife."]

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

newtype First' a
    = First' { getFirst' :: Optional a }
    deriving (Eq, Show)

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
    a <- arbitrary
    frequency
        [(1, return First' { getFirst' = Nada })
        , (1, return First' { getFirst' = Only a })]

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = genFirst

-- when we withhold the monoid instance for a (i.e. no Monoid a => )
-- in order to typecheck we can't a <> b. so we end up with a monoid instance
-- that just gets the first success value
instance Monoid (First' a) where
    mempty = First' Nada

    mappend (First' Nada) a = a
    mappend a (First' Nada) = a
    mappend a b             = a

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend
    = First' String -> First' String -> First' String -> Bool

type FstId
    = First' String -> Bool

testFirst :: IO ()
testFirst = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
