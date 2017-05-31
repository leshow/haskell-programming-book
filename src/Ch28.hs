module Main where

import           Criterion.Main

infixl 9 !?
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n < 0 = Nothing
    | otherwise =
        foldr (\x r k -> case k of
            0 -> Just x
            _ -> r (k-1)) (const Nothing) xs n


-- this is the type inferred by GHC leaving it with constrained typeclasses
-- makes it not able to be fully applied when compiled, and therefore unable
-- to be compiled into a tight loop.
infixl 9 !?!
{-# INLINABLE (!?!) #-}
(!?!) :: (Foldable t, Ord b, Num b) => t a -> b -> Maybe a
xs !?! n
    | n < 0 = Nothing
    | otherwise =
        foldr (\x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)) (const Nothing) xs n

myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain
    [ bench "index list 9999"
        $ whnf (myList !!) 9998
    , bench "index list maybe index 9999"
        $ whnf (myList !?) 9998
    , bench "not fully applied bench, index 9999"
        $ whnf (myList !?!) 9998
    , bench "compare to nf"
        $ nf (myList !!) 9998
    ]

