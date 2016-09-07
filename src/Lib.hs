module Lib where

fstString :: String -> String
fstString x = x ++ " in the rain"

sndString :: String -> String
sndString x = x ++ " over the rainbow"

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

ex1 = max (length [1,2,3]) (length [8,9,10,11,12])
ex2 = compare (3*4) (3*5)
--ex3 = compare "Julie" True -- compare :: Ord a => a -> a -> Ordering -- here a is not the same type. won't compile.
ex4 = (5+3) > (3+6)

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

