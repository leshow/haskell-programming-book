module Main where


import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception
import           Data.Monoid        ((<>))
import           System.IO


openAndWrite :: IO ()
openAndWrite = do
    h <- openFile "test.dat" WriteMode
    threadDelay 1500
    hPutStr h (replicate 100000000 '0' <> "abc")
    hClose h

data PleaseDie = PleaseDie deriving (Show)

instance Exception PleaseDie


main :: IO ()
main = do
    tid <- forkIO openAndWrite
    threadDelay 1000000
    throwTo tid PleaseDie
