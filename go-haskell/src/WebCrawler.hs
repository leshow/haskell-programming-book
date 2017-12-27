{-# LANGUAGE RecordWildCards #-}

module WebCrawler where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, async, cancel,
                                           forConcurrently_, wait, withAsync)
import           Control.Concurrent.STM   (TQueue, TVar, atomically,
                                           modifyTVar', newTQueue, newTVar,
                                           readTQueue, readTVar, writeTQueue)
import           Control.DeepSeq          (deepseq)
import           Control.Monad            (forM, forM_, forever, unless, when)
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
                forM_ urls $ \u -> crawlNaive u (depth - 1) f

crawl :: Fetchable f => String -> Int -> f -> TQueue String -> TVar (Set String) -> IO ()
crawl url depth f out cache
    | depth <= 0 = pure ()
    | otherwise = do
        new <- atomically $ do
            set <- readTVar cache
            let here = Set.member url set
            unless here $ modifyTVar' cache (Set.insert url)
            pure here
        unless new $ do
            res <- fetch f url
            case res of
                Error s -> atomically $ writeTQueue out s
                Ok {..} -> do
                    atomically $ writeTQueue out ("found: " <> url <> " \"" <> body <> "\"")
                    forConcurrently_ urls $
                        \u -> crawl u (depth - 1) f out cache

main :: IO ()
main = do
    out <- atomically newTQueue :: IO (TQueue String)
    cache <- atomically $ newTVar Set.empty
    reader <- async $ readAndPrint out
    crawl "http://golang.org/" 4 fetcher out cache
    threadDelay $ 10^6
    cancel reader

readAndPrint :: TQueue String -> IO ()
readAndPrint out = forever $ do
    s <- atomically (readTQueue out)
    putStrLn s



mainSerial :: IO ()
mainSerial = crawlNaive "http://golang.org/" 4 fetcher


data FakeResult = FakeResult {
    fakeBody   :: String
    , fakeUrls :: [String]
    } deriving (Show)

newtype FakeFetcher = FakeFetcher {
        getMap :: Map String FakeResult
    }

instance Fetchable FakeFetcher where
    fetch f url = case Map.lookup url (getMap f) of
        Nothing                     -> pure $ Error ("Not found: " <> url)
        Just (FakeResult body urls) -> pure $ Ok body urls

fetcher :: FakeFetcher
fetcher = FakeFetcher $ Map.fromList [
        ( "http://golang.org/"
        , FakeResult
            "The Go Programming Language"
            [ "http://golang.org/pkg/"
            , "http://golang.org/cmd/"
            ]
        )
        , ( "http://golang.org/pkg/"
        , FakeResult
            "Packages"
            [ "http://golang.org/"
            , "http://golang.org/cmd/"
            , "http://golang.org/pkg/fmt/"
            , "http://golang.org/pkg/os/"
            ]
        )
        , ( "http://golang.org/pkg/fmt/"
        , FakeResult
            "Packages fmt"
            [ "http://golang.org/"
            , "http://golang.org/pkg/"
            ]
        )
        , ( "http://golang.org/pkg/os/"
        , FakeResult
            "Packages os"
            [ "http://golang.org/"
            , "http://golang.org/pkg/"
            ]
        )
    ]
