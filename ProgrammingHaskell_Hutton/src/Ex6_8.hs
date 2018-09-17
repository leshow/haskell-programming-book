module Ex6_8 where

conc :: [[a]] -> [a]
conc = foldr (++) []

concc :: [[a]] -> [a]
concc []       = []
concc (x : xs) = x ++ conc xs

replicate_ :: Int -> a -> [a]
replicate_ 0 _ = []
replicate_ n a = a : replicate (n - 1) a

(!!*) :: [a] -> Int -> a
[x] !!* 0 = x
(!!*) (_ : xs) n = (!!*) xs (n - 1)

elem_ :: Eq a => a -> [a] -> Bool
elem_ _ [] = False
elem_ e (x : xs) | x == e    = True
                 | otherwise = elem_ e xs

merge_ :: Ord a => [a] -> [a] -> [a]
merge_ [] ys = ys
merge_ xs [] = xs
merge_ (x : xs) (y : ys) | x <= y    = x : merge_ xs (y : ys)
                         | otherwise = y : merge_ (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take len xs, drop len xs) -- (,) <$> take len <*> drop len
    where len = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = let (s, e) = halve xs in merge_ (msort s) (msort e)
