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
