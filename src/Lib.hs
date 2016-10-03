-- {-# LANGUAGE RecordWildCards #-}

module Lib where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Char
import           Data.IORef

fstString :: String -> String
fstString x = x ++ " in the rain"

sndString :: String -> String
sndString x = x ++ " over the rainbow"

sing :: String
sing = if length x > length y then fstString x else sndString y
    where   x = "Singing"
            y = "Somewhere"

printie :: IO ()
printie = do
    print (1+2)
    print 10
    print (negate (-1))
    print ((+) 0 blah)
    where
        blah = negate 1

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (a, b) = (xz a, yz b)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge a b x = fst $ b (a x)

--ex3 = compare "Julie" True -- compare :: Ord a => a -> a -> Ordering -- here a is not the same type. won't compile.

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
        (==) Mon Mon = True
        (==) Tue Tue = True
        (==) Wed Wed = True
        (==) Thu Thu = True
        (==) Fri Fri = True
        (==) Sat Sat = True
        (==) Sun Sun = True
        (==) _ _     = False

instance Eq Date where
        (==) (Date weekday dayOfMonth)
            (Date weekday' dayOfMonth') =
                    weekday == weekday' && dayOfMonth == dayOfMonth'

--ch6
--
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
        (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
        (==) (Two a b) (Two x y) = a == b && x == y

data StringOrInt
    = TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
        (==) (TisAnInt a) (TisAnInt b)     = a == b
        (==) (TisAString a) (TisAString b) = a == b
        (==) _ _                           = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
        (==) (Pair x y) (Pair m n) = x == m && y == n

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
        (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a
    = ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
        (==) (ThisOne a) (ThisOne b) = a == b
        (==) (ThatOne a) (ThatOne b) = a == b
        (==) _ _                     = False

data EitherOr a b
    = Hello a
    | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
        (==) (Hello a) (Hello b)     = a == b
        (==) (GoodBye a) (GoodBye b) = a == b
        (==) _ _                     = False

data NewThing a
    = One a
    | Other a a
    deriving (Show)

instance Functor NewThing where
    fmap f (One a)     = One (f a)
    fmap f (Other a b) = Other (f a) (f b)

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson = print

data Mood
    = Blah
    | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
                  else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
        deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String
        deriving (Eq, Show)

data Yeah = Yeah Bool
        deriving (Eq, Show)

data Papu = Papu Rocks Yeah
        deriving (Eq, Show)

phew = Papu (Rocks "chases") (Yeah True)

rf :: RealFrac a => a
rf = 1.0

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f a b = f b + fromInteger a

bindExp :: Integer -> String
bindExp x = let
                y = 5
            in
                "integer was: " ++ show x ++ " and y was: " ++ show y

addOneIfOdd n = if odd n then f n else n
                where f = (+ 1)

data WherePenguinsLive
    = Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive
        deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

penguinLocation :: Penguin -> WherePenguinsLive
penguinLocation (Peng location) = location

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False


k :: (a, b) -> a
k (x, y) = x

k2 :: String
k2 = k ("three", 1+2)

k1 :: Num a => a
k1 = k (4-1, 10)

k3 :: Num a => a
k3 = k (3, True)

f2 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f2 (a, b, c) (d, e, f) = ((a, d),(c,f))

ifEvenAdd2 n = case compare n 0 of
                 LT -> -1
                 GT -> 1
                 EQ -> 0

myAbs :: Integer -> Integer
myAbs x
  | x < 0       = -x
  | otherwise   = x

testIORef = do
        box <- newIORef (0 :: Int)
        alterRef box
        readIORef box >>= print
        modifyIORef box (+1)
        readIORef box >>= print

alterRef ref = modifyIORef ref (+1)

testMVar = do
        m <- newEmptyMVar
        forkIO $ do
                v <- takeMVar m -- this thread will block until it gets a value from this mvar
                putStrLn ("reveived " ++ show v)
        putStrLn "sending"
        putMVar m "suck this" -- send value through channel

myWords str = go str []
    where word = takeWhile (/=' ')
          rest = dropWhile (/=' ')
          go x xs
            | null x = xs
            | head x == ' ' = go (drop 1 x) xs
            | otherwise = go (rest x) (xs ++ [word x])


mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tuplesOf = [(x,y) | x <- mySqr, y <- myCube]
tuplesLessThan = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
tuplesLn = length tuplesLessThan

itIsMystery = map (`elem` "aeiou") -- is a vowel

multipleOfThree = filter (\x -> x `mod` 3 == 0) [1..30]

lengthOf = length . filter (\x -> x `mod` 3 == 0)

naughtyWords = ["the", "a", "an"]

myFilter = filter (`notElem` naughtyWords) . words
