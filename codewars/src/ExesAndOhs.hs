module ExesAndOhs where

import           Data.Char

-- | Returns true if the number of
-- Xs is equal to the number of Os
-- (case-insensitive)
xo :: String -> Bool
xo str = let
            newstr = fmap toLower str
            xs = length (filter (=='x') newstr)
            os = length (filter (=='o') newstr)
        in
            xs == os
