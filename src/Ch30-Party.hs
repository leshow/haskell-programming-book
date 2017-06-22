module Main where

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad      (forever)
import           System.Random      (randomRIO)

randomException :: IO ()
randomException = do
    i <- randomRIO (1, 10 :: Int)
    if i `elem` [1..9]
        then throwIO DivideByZero
        else throwIO StackOverflow

main :: IO ()
main = forever $ do
    let tryS :: IO () -> IO (Either SomeException ()) -- modified from ArithExceptions so that it captures all Exeptions
        tryS = try
    _ <- tryS randomException
    putStrLn "Live to loop another day!"
    -- microseconds
    threadDelay (1 * 1000000)
