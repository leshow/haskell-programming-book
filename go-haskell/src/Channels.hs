module Channels where


import           Control.Concurrent       (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM   (TQueue, atomically, newTQueue,
                                           readTQueue, writeTQueue)
import qualified Data.List                as L
import           Prelude                  hiding (sum)


sum :: Num a => [a] -> Chan a -> IO ()
sum xs chan = writeChan chan (L.sum xs)

half :: (Int -> [a] -> [a]) -> [a] -> [a]
half f xs = f (length xs `div` 2) xs

main :: IO ()
main = do
    let s = [8, 2, 4, 0, 1, -1, -3]
    c <- newChan :: IO (Chan Int)
    async $ sum (half drop s) c
    async $ sum (half take s) c
    [x, y] <- sequence [readChan c, readChan c]
    print (x, y, x+y)

{- STM version -}

sumBySTM :: Num a => [a] -> TQueue a -> IO ()
sumBySTM xs chan = atomically $ writeTQueue chan (L.sum xs)

stmMain :: IO ()
stmMain = do
    let s = [8, 2, 4, 0, 1, -1, -3]
    c <- atomically newTQueue :: IO (TQueue Int)
    async $ sumBySTM (half drop s) c
    async $ sumBySTM (half take s) c
    [x, y] <- sequence $ fmap atomically [readTQueue c, readTQueue c]
    print (x, y, x+y)
