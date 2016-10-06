{-# LANGUAGE OverloadedStrings #-}

module Ch12 where

import           Data.List
import           Data.Text as T (Text, intercalate, splitOn)

{-
    HKT are types that take types as arguments, like higher order functions.
    Haskell Report calls HKT "type constructors"
    Haskell Report calls fully applied types "type constants"

    * is the kind of all standard lifted types.
    unlifted types are of kind # and represent mostly pointers and native machine types

    type constructors are just functions, and can be used like functions
-}
makeJust = fmap Just [1, 2, 3]
-- [Just 1,Just 2,Just 3]

{-
    Ch Exercises
    1. :k  of a in id :: a -> a
    a :: *
    2. :k of a and f in r :: a -> f a
    a :: * , f :: * -> *
-}

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a     = Just a

getReplacement :: Maybe String -> String
getReplacement Nothing  = "a"
getReplacement (Just a) = a

replaceThe :: Text -> Text
replaceThe = T.intercalate "a" . T.splitOn "the"

replaceThe' :: String -> String
replaceThe' sent = unwords . map (\x -> if x == "the" then "a" else x) $ words sent

replaceThe'' :: String -> String
replaceThe'' sent = unwords . repl $ words sent
    where
        repl []     = []
        repl (x:xs) = if x == "the" then "a":repl xs else x:repl xs

--countTheBeforeVowel :: String -> Integer
--countTheBeforeVowel sent = words sent

countVowels :: String -> Integer
countVowels = go
    where
        go ""     = 0
        go (x:xs) = (if x `elem` ("aeiou"::String) then 1 else 0) + go xs

newtype Word' = Word' String
    deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

countVC :: String -> (Int, Int)
countVC "" = (0, 0)
countVC (x:xs) = (v + fst (countVC xs), c + snd (countVC xs))
    where
        (v,c) = if x `elem` vowels then (1,0) else (0,1)

mkWord :: String -> Maybe Word'
mkWord str
    | v > c = Just $ Word' str
    | otherwise = Nothing
    where
        (v,c) = countVC str

-- Nat

data Nat
    = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | n >= 0 = Just $ succit n
    where
        succit :: Integer -> Nat
        succit 0 = Zero
        succit n = Succ $ succit (n-1)


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing  = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a
