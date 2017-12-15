module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async)
import           Data.Foldable            (for_)

say :: String -> IO ()
say s = for_ [0..4] $ \_ -> do
    threadDelay 3000000
    putStrLn s

main :: IO ()
main = do
    async $ say "world"
    say "hello"
