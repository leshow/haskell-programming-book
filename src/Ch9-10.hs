module Ch9n10 where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char

eft
    :: (Enum a, Ord a)
    => a -> a -> [a]
eft start stop
    | start < stop = start : eft (succ start) stop
    | start == stop = [stop]

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftChar :: Char -> Char -> String
eftChar = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

newZip = myZipWith (\x y -> (x, y))

filterUpper = filter isUpper

capitaliseFirst :: String -> String
capitaliseFirst [] = []
capitaliseFirst (x:xs) = toUpper x : xs

capitaliseAll :: String -> String
capitaliseAll [] = []
capitaliseAll (x:xs) = toUpper x : capitaliseAll xs

-- ||
capAll = map toUpper

capFirst = toUpper . head

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- myOr [] = False
-- myOr (x:xs) = x || myOr xs
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

-- \x y -> (||) (f x) y
-- \x -> (||) . f x
-- (||) . f
myElem
    :: Eq a
    => a -> [a] -> Bool
myElem el = any (el ==)

-- with recursion
-- myRElem :: Eq a => a -> [a] -> Bool
-- myRElem el [] = False
-- myRElem el (x:xs) = el == x || myRElem el xs
-- with fold
-- myFElem :: Eq a => a -> [a] -> Bool
-- myFElem el = foldr (\x y -> el == x || y) False
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- myReverse = foldl (\x y -> y : x) []
squish :: [[a]] -> [a]
squish = foldr (++) []

foldrMap :: (a -> b) -> [a] -> [b]
foldrMap f = foldr (\x xs -> f x : xs) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x xs -> f x ++ xs) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:y:[]) = gtr f x y -- ghc wants me to use [x, y]
myMaximumBy f (x:ys) = gtr f x (myMaximumBy f ys)

gtr :: (a -> a -> Ordering) -> a -> a -> a
gtr f a b =
    if f a b == GT
        then a
        else b

lt :: (a -> a -> Ordering) -> a -> a -> a
lt f a b =
    if f a b == LT
        then a
        else b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f [x, y] = lt f x y
myMinimumBy f (x:ys) = lt f x (myMinimumBy f ys)

myMaxFold :: (a -> a -> Ordering) -> [a] -> a
myMaxFold f (x:xs) = foldr cmp x xs
  where
    cmp a b =
        if f a b == GT
            then a
            else b

myMinFold :: (a -> a -> Ordering) -> [a] -> a
myMinFold f (x:xs) = foldr cmp x xs
  where
    cmp a b =
        if f a b == LT
            then a
            else b

myMaximum
    :: Ord a
    => [a] -> a
myMaximum = myMaxFold compare

myMinimum
    :: Ord a
    => [a] -> a
myMinimum = myMinFold compare

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs = 1 : scanl (+) 1 fibs

fibszip = 1 : 1 : zipWith (+) fibszip (tail fibszip)

fibsN x = fibs !! x

fibs20 = take 20 fibs

fibsLT = takeWhile (< 100) fibs

factorial 0 = 1
factorial n = n * factorial (n - 1)

fact = drop 2 $ 1 : scanl (*) 1 [1 ..] -- i dont know how to write it without drop 2

stops = "pbtdkg"

vowels = "aeiou"

mkTup :: [a] -> [b] -> [c] -> [(a, b, c)]
mkTup x y z =
    [ (a, b, c)
    | a <- x 
    , b <- y 
    , c <- z ]

svs = mkTup stops vowels stops

fstt (a, b, c) = a

withP = filter ((==) 'p' . fstt) svs

withPTup =
    [ (a, b, c)
    | a <- stops 
    , b <- vowels 
    , c <- stops 
    , a == 'p' ]

nouns = ["computer", "desk", "sky", "Haskell"]

verbs = ["type", "smack", "snort", "run"]

sentences = mkTup nouns verbs nouns

-- do
--     a <- stops
--     b <- vowels
--     c <- stops
--     return (a,b,c)
-- λ> :t liftA3
-- liftA3
--   :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- λ> :t (,,)
-- (,,) :: a -> b -> c -> (a, b, c)
mkTup2 = liftA3 (,,) stops vowels stops

seekritFunc x = div (sum (map length (words x))) (length (words x))

-- avg word length
fracSeekrit x =
    fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

myFFilter :: (a -> Bool) -> [a] -> [a]
myFFilter f =
    foldr
        (\x xs ->
              if f x
                  then x : xs
                  else xs)
        []
