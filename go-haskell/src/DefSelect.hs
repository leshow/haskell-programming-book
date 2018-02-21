{-# LANGUAGE UnicodeSyntax #-}

module DefSelect where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, cancel)
import           Control.Concurrent.STM   (STM, TQueue, atomically, newTQueue,
                                           orElse, readTQueue, writeTQueue)
import           Control.Monad            (forever, guard, join, msum)
import           Data.Monoid              ((<>))

select :: [STM a] -> IO a
select = atomically . msum


newTicker :: Int -> IO (TQueue (), IO ())
newTicker ms = do
    ch <- atomically newTQueue
    handle <- async $ forever $ do
        threadDelay (ms*100)
        async $ atomically $ writeTQueue ch ()
    pure (ch, cancel handle)

newAfter :: Int -> IO (TQueue (), IO ())
newAfter ms = do
    ch <- atomically newTQueue
    handle <- async $ do
        threadDelay (ms*100)
        atomically $ writeTQueue ch ()
    pure (ch, cancel handle)

main :: IO ()
main = do
    (tick, _) <- newTicker 3000
    (boom, _) <- newAfter 5000
    loop tick boom
    where
        loop tick boom =
            join $ select [
                readTQueue boom >> pure (putStrLn "BOOM!")
                , readTQueue tick >> pure (do
                    putStrLn "tick."
                    loop tick boom)
                , pure $ do
                    putStrLn "    ."
                    threadDelay 50000
                    loop tick boom
                ]


fizzbuzz :: [String]
fizzbuzz = do
    x ← [1..100]
    pure $ concat (fizz x <> buzz x <> num x)
    where
        fizz, buzz, num :: Int -> [String]
        fizz n = "fizz" <$ guard (n `mod` 3 == 0)
        buzz n = "buzz" <$ guard (n `mod` 5 == 0)
        num n = show n <$ guard (null (fizz n) && null (buzz n))

-- getGoodDogs :: [Person] -> [String]
-- getGoodDogs people =
--     [ name ++ " is a good dog."  -- all dogs are good
--     | Person { personPets = pets } <- people
--     , Pet { petName = Just name, petType = Dog } <- pets
--     ]
