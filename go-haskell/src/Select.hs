module Select where

import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM   (STM, TQueue, atomically, newTQueue,
                                           orElse, readTQueue, writeTQueue)
import           Control.Monad            (forM_, join)


select :: [STM a] -> IO a
select stms = atomically $ foldl1 orElse stms -- atomically . msum -- uses MonadPlus and Alternative instances

fibonacci :: TQueue Int -> TQueue () -> IO ()
fibonacci ch quit = loop 0 1
    where
        loop start step = join $ select
            [ writeTQueue ch start >> pure (loop step (start+step))
            , readTQueue quit >> pure (print "quit")
            ]
