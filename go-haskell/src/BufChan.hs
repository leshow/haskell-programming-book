module BufChan where

import           Control.Concurrent.STM (TBQueue, atomically, newTBQueue,
                                         readTBQueue, writeTBQueue)

mainBySTM :: IO ()
mainBySTM = do
    ch <- atomically $ newTBQueue 2 :: IO (TBQueue Int)
    atomically $ writeTBQueue ch 1
    atomically $ writeTBQueue ch 2
    print =<< atomically (readTBQueue ch)
    print =<< atomically (readTBQueue ch)
