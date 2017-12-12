module BoundedChannels where


import           Control.Concurrent.Async       (async)
import           Control.Concurrent.BoundedChan (BoundedChan, getChanContents,
                                                 newBoundedChan, readChan,
                                                 writeChan)
import           Control.Monad                  (foldM_, forM_)
import           Control.Monad                  (guard)
import           Data.Maybe                     (catMaybes, isJust)

--range :: TBQueue (Maybe a) -> IO [a]

