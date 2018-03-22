{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async)
import           Data.Foldable            (for_)
import           Data.Monoid              ((<>))

say :: String -> IO ()
say s = for_ [0..4] $ \_ -> do
    threadDelay 3000000
    putStrLn s

main :: IO ()
main = do
    async $ say "world"
    say "hello"

-- qs :: ∀ a. Ord a ⇒ [a] → [a]
qs :: forall a. Ord a => [a] -> [a]
qs [] = []
qs (pivot:xs) =
    let left    = qs [y | y <- xs, y <=  pivot]
        right   = qs [y | y <- xs, y > pivot]
    in
        left <> [pivot] <> right
