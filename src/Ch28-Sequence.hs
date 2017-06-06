module Main where


import           Criterion.Main
import qualified Data.Sequence  as S

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [S.Seq Int]
seqs = replicate 10 (S.fromList [1..100000])

lists' :: [Int]
lists' = [1..100000]

seqs' :: S.Seq Int
seqs' = S.fromList [1..100000]

main :: IO ()
main = defaultMain
    [ bench "concat lists" $ nf mconcat lists
    , bench "concat seq" $ nf mconcat seqs
    , bench "index list" $ whnf (\x -> x !! 9001) lists'
    , bench "index sequence" $ whnf (\x -> S.index x 9001) seqs' -- whnf (`S.index` 9001) seqs' -- whnf (flip S.index 9001) seqs'
    ]

