module Main where


import           Control.Monad.Primitive
import           Control.Monad.ST
import           Criterion.Main
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as MV

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
    mvec <- GM.new (n+1)
    iterateM mvec n
    where
        iterateM v 0 = return v
        iterateM v n = MV.write v n 0 >> iterateM v (n-1)

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
    mvec <- GM.new (n+1)
    iterateM mvec n
    where
        iterateM v 0 = V.freeze v
        iterateM v n = MV.write v n 0 >> iterateM v (n-1)

main :: IO ()
main = defaultMain
    [ bench "mutable IO vec" $ whnfIO (mutableUpdateIO 9998)
    , bench "mutable ST vec" $ whnf mutableUpdateST 9998
    ]
