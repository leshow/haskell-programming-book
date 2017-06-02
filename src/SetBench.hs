{-# LANGUAGE ExplicitForAll #-}

module Main where


import           Criterion.Main
import qualified Data.Map       as M
import qualified Data.Set       as S

bumpIt :: forall a. Num a => (a,a) -> (a,a)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
    where stream = iterate bumpIt (0,0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
    where stream = iterate (+1) 0

insertNewS :: Int -> S.Set Int
insertNewS x = S.insert x s

insertNewM :: Int -> M.Map Int Int
insertNewM x = M.insert x x m

s' :: S.Set Int
s' = S.fromList $ take 5000 stream
    where stream = iterate (+1) 1000

m' :: M.Map Int Int
m' = M.fromList $ take 5000 stream
    where stream = iterate bumpIt (0,0)


main :: IO ()
main = defaultMain
    [ bench "bench 10000 Map" $ whnf (`M.member` m) 9999
    , bench "bench member 10000 Set" $ whnf (`S.member` s) 9999
    , bench "insert set 10001" $ whnf insertNewS 10001
    , bench "insert map 10001" $ whnf insertNewM 10001
    , bench "set union overlap" $ whnf (S.union s) s'
    , bench "map union overlap" $ whnf (M.union m) m'
    ]
