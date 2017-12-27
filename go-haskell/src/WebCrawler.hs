{-# LANGUAGE RecordWildCards #-}

module WebCrawler where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, async, cancel,
                                           forConcurrently_, wait, withAsync)
import           Control.Concurrent.STM   (TQueue, TVar, atomically,
                                           modifyTVar', newTQueue, newTVar,
                                           readTQueue, readTVar, writeTQueue)
import           Control.DeepSeq          (deepseq)
import           Control.Monad            (forM, forM_, forever, when)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Monoid              ((<>))
import           Data.Set                 (Set)
import qualified Data.Set                 as Set

data Result
    = Ok { body :: String, urls :: [String] }
    | Error String
    deriving Show

class Fetchable a where
    fetch :: a -> String -> IO Result

{- crawlNaive is a serial impl of crawl -}
crawlNaive :: Fetchable f => String -> Int -> f -> IO ()
crawlNaive url depth f
    | depth == 0 = pure ()
    | otherwise  = do
        res <- fetch f url
        case res of
            Error s -> putStrLn s
            Ok {..} -> do
                putStrLn $ "found: " <> url <> " \"" <> body <> "\""
                forM_ urls $ \url -> do
                    crawlNaive url (depth - 1) f
