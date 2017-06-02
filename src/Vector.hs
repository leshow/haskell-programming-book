module Main where


import           Criterion.Main
import qualified Data.Vector    as V

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take to (drop from xs)

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList [1..1000]

main :: IO ()
main = defaultMain
    [ bench "slicing list" $ whnf (head . slice 100 900) l
    , bench "slicing vec" $ whnf (V.head . V.slice 100 900) v
    ]
