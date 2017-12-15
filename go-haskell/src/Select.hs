module Select where

import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM   (STM, TQueue, atomically, newTQueue,
                                           orElse, readTQueue, writeTQueue)
import           Control.Monad            (join, msum)
import           Data.Foldable            (for_)


select :: [STM a] -> IO a
select = atomically . msum --select stms = atomically $ foldl1 orElse stms -- atomically . msum -- uses MonadPlus and Alternative instances

fibonacci :: TQueue Int -> TQueue () -> IO ()
fibonacci ch quit = loop 0 1
    where
        loop start step = join $ select
            [ writeTQueue ch start >> pure (loop step (start+step))
            , readTQueue quit >> pure (print "quit")
            ]

main :: IO ()
main = do
    ch <- atomically newTQueue
    quit <- atomically newTQueue
    async $ do
        for_ [0..9] $ \_ -> do
            x <- atomically $ readTQueue ch
            print x
        atomically $ writeTQueue quit ()
    fibonacci ch quit
    pure ()
