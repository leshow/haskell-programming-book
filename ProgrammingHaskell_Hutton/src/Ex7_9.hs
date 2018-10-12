module Ex7_9 where

import           Data.Char
import           Data.Foldable
import           Control.Monad
import           Debug.Trace
import           Data.Function
-- Exercises:

-- [f x | x <- xs, p x]
filtmap :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtmap f p = map f . filter p

all_ :: (a -> Bool) -> [Bool] -> Bool -- ? I think they mean: (a -> Bool) -> [a] -> Bool
all_ _ xs = foldr (&&) True xs

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (&&) True . fmap f

any_ :: (a -> Bool) -> [a] -> Bool
any_ f = foldr (||) False . fmap f

takeWhile_ :: (a -> Bool) -> [a] -> [a]
takeWhile_ _ []       = []
takeWhile_ f (x : xs) = if f x then x : takeWhile_ f xs else []

dropWhile_ :: (a -> Bool) -> [a] -> [a]
dropWhile_ _ []       = []
dropWhile_ f (x : xs) = if f x then dropWhile_ f xs else xs

map_ :: (a -> b) -> [a] -> [b]
map_ f = foldr (\a acc -> f a : acc) []

filter_ :: (a -> Bool) -> [a] -> [a]
filter_ f = foldr (\a acc -> if f a then a : acc else acc) []

dec2int :: [Int] -> Int
dec2int = foldl (\acc a -> 10 * acc + a) 0

curry_ :: ((a, b) -> c) -> a -> b -> c
curry_ f a b = f (a, b)

uncurry_ :: (a -> b -> c) -> (a, b) -> c
uncurry_ f (a, b) = f a b

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail


iterate_ :: (a -> a) -> a -> [a]
iterate_ = unfold (const False) id


chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

make8 :: [Bit] -> [Bit]
make8 = take 8 . flip (<>) (repeat 0)

make9 :: [Bit] -> [Bit]
make9 b = take 8 (b <> repeat 0) <> evn b

evn :: (Integral a, Foldable t) => t a -> [Int]
evn b = if even . sum $ b then [0] else [1]

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits =
    let line   = take 9 bits
        parity = sum line
        bit    = last line
    in  if parity /= bit
            then error "Parity bit mismatch"
            else init line : chop8 (drop 9 bits)


transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = fmap (\(h, a) -> h a) . zip (fgs [f, g])
    where fgs = join . repeat

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

decodeParity :: [Bit] -> String
decodeParity = map (chr . bin2int) . chop9

encode :: String -> [Bit]
encode = concatMap $ make8 . int2bin . ord

encodeParity :: String -> [Bit]
encodeParity = concatMap $ make9 . int2bin . ord


-- Random Chapter work:
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\(i, b) bs -> i * b + bs) 0 . zip (iterate (* 2) 1)
-- book: bin2int bits = sum [w * b | (w,b) <- zip (iterate (*2) 1) bits]

-- written in proper direction
hex2int :: [Char] -> Int
hex2int hex = sum
    [ w * getInt b | (w, b) <- zip (iterate (* 16) 1) (reverse hex) ]
  where
    getInt c =
        let c' = ord c in if c' >= 65 then c' - ord 'A' + 10 else c' - ord '0'

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

insert_ :: Ord a => [a] -> a -> [a]
insert_ []       x = [x]
insert_ (x : xs) y = if y < x then y : insert_ xs x else x : insert_ xs y

isort :: Ord a => [a] -> [a]
isort []       = []
isort (x : xs) = insert_ (isort xs) x

rmDupes :: Eq a => [a] -> [a]
rmDupes [] = []
rmDupes (x : xs) =
    let !ys = rmDupes xs
        !fx = filter (/= x)
    in  x : fx ys

{-
A Narcissistic Number is a number of length n in which the sum of its digits to the power of n is equal to the original number. If this seems confusing, refer to the example below.

Ex: 153, where n = 3 (number of digits in 153)
13 + 53 + 33 = 153

Write a method is_narcissistic :: Integer -> Bool
-}
isNarcissistic :: Integer -> Bool
isNarcissistic n = total (len n) n == n
  where
    total :: Integer -> Integer -> Integer
    total _      0  = 0
    total !count !c = (c `mod` 10) ^ count + total count (c `div` 10)

len :: Integer -> Integer
len 0  = 0
len !t = 1 + len (t `div` 10)

same :: (Floating a, Integral a, Eq a) => [a] -> [a] -> [Bool]
same x y = getSqrt x y
  where
    getSqrt x' y' = do
        a <- x'
        b <- y'
        guard $ sqrt b == a
        pure True
