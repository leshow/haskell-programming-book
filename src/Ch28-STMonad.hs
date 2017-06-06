module Main where


--import           Control.Monad.Primitive
import           Control.Monad.ST
import           Criterion.Main
import           Data.Foldable               (for_)
import           Data.STRef                  (modifySTRef, newSTRef, readSTRef,
                                              writeSTRef)
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as MV
import           Prelude                     hiding ((!))

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
    mvec <- GM.new (n+1)
    iterateM mvec n
    where
        iterateM v 0 = return v
        iterateM v i = MV.write v i 0 >> iterateM v (i-1)

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
    mvec <- GM.new (n+1)
    iterateM mvec n
    where
        iterateM v 0 = V.freeze v
        iterateM v i = MV.write v i 0 >> iterateM v (i-1)

main :: IO ()
main = defaultMain
    [ bench "mutable IO vec" $ whnfIO (mutableUpdateIO 9998)
    , bench "mutable ST vec" $ whnf mutableUpdateST 9998
    ]

add = runST $ do
    ref <- newSTRef 0
    for_ [1..10] $ \a ->
        modifySTRef ref (+a)
    readSTRef ref
