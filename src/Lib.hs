module Lib where

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

data StringOrInt =
        TisAnInt Int
          | TisAString String
instance Eq StringOrInt where
        (==) (TisAnInt a) (TisAnInt b) = a == b
        (==) (TisAString a) (TisAString b) = a == b
        (==) _ _ = False

data Pair a =
        Pair a a

instance Eq a => Eq (Pair a) where
        (==) (Pair x y) (Pair m n) = x == m && y == n

data Tuple a b =
        Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
        (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a =
        ThisOne a
          | ThatOne a

instance Eq a => Eq (Which a) where
        (==) (ThisOne a) (ThisOne b) = a == b
        (==) (ThatOne a) (ThatOne b) = a == b
        (==) _ _ = False

data EitherOr a b =
        Hello a
          | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
        (==) (Hello a) (Hello b) = a == b
        (==) (GoodBye a) (GoodBye b) = a == b
        (==) _ _ = False


