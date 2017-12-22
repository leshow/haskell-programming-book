module SyncMutex where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM   (TVar, atomically, modifyTVar',
                                           newTVar, readTVar)
import           Control.Monad            (forM_)
import           Data.Map.Strict          (Map, (!))
import qualified Data.Map.Strict          as Map

newtype SafeCounter = SafeCounter { v :: TVar (Map String Int) }

newSafeCounter :: Map String Int -> IO SafeCounter
newSafeCounter m = SafeCounter <$> atomically (newTVar m)

inc :: SafeCounter -> String -> IO ()
inc c key = atomically $ modifyTVar' (v c) (Map.alter inc_ key)
    where
        inc_ Nothing  = Just 1
        inc_ (Just x) = Just $ x+1

value :: SafeCounter -> String -> IO Int
value c key = fmap (Map.findWithDefault 0 key) (atomically (readTVar (v c)))


main :: IO ()
main = do
    c <- newSafeCounter Map.empty
    forM_ [1..1000] $ \_ -> async $ inc c "key"
    threadDelay (10^6 :: Int)
    val <- value c "key"
    print val
