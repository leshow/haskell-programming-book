module BoundedChannels where


import           Control.Concurrent.Async       (async)
import           Control.Concurrent.BoundedChan (BoundedChan, getChanContents,
                                                 newBoundedChan, readChan,
                                                 writeChan)
import           Control.Monad                  (foldM_)
import           Data.Foldable                  (for_)
import           Data.Maybe                     (catMaybes, isJust)

range :: BoundedChan (Maybe a) -> IO [a]
range chan = do
    xs <- getChanContents chan
    pure $ catMaybes (takeWhile isJust xs)

fibonnacci :: Int -> BoundedChan (Maybe Int) -> IO ()
fibonnacci n chan = do
    foldM_ iter (0, 1) [0..n] -- send fibs up to n
    writeChan chan Nothing  -- write done
    where
        iter :: (Int, Int) -> t -> IO (Int, Int)
        iter (x, y) _ = do
            writeChan chan (Just x)
            pure (y, x + y)

main :: IO ()
main = do
    let size = 10
    ch <- newBoundedChan size
    async $ fibonnacci size ch
    ch' <- range ch
    for_ ch' print
    pure ()


{- implemented as a regular list -}
fibonacciByList :: Int -> [Int]
fibonacciByList n = (fib !!) <$> [0..n]
    where
        fib = 0 : 1 : zipWith (+) fib (tail fib)

fibList :: Int -> [Int]
fibList n = fib n 0 1
    where
        fib :: Int -> Int -> Int -> [Int]
        fib 0 a _ = [a]
        fib t a b = a : fib (t-1) b (a + b)

mainByList :: IO ()
mainByList = do
    let size = 10
        fibs = fibonacciByList size
    for_ fibs print
    pure ()
