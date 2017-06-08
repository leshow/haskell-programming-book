module Main where

import           Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar,
                                     takeMVar, threadDelay)

main :: IO ()
main = do
    mv <- newEmptyMVar
    putMVar mv (0 :: Int)
    forkIO (forked mv)
    threadDelay 1000
    a <- takeMVar mv
    print a
    a' <- takeMVar mv
    print a'

forked :: Num a => MVar a -> IO ()
forked mv = do
    a <- takeMVar mv
    putMVar mv (a+1)
    threadDelay 1000000
    putMVar mv (a+1)
    return ()

