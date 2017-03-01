module Ch22 where


import           Control.Applicative
import           Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop -- fmap boop doop x == (*2) ((+10) x)

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop
-- in these 2 scenarios, the argument will get passed to both boop and doop in parallel and the results added together
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = liftA2 (,) cap rev

tupled' :: String -> (String, String)
tupled' = do
    c <- cap
    r <- rev
    return (c, r)

tupled'' :: String -> (String, String)
tupled'' = (,) <$> cap <*> rev

tupled''' :: String -> (String, String)
tupled''' = rev <$> cap >>= (,)
