module Main where

import           Control.Monad (replicateM_)

blah :: [Integer]
blah = [1..1000]


main :: IO ()
main = replicateM_ 10000 (print blah)
