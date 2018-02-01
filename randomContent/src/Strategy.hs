module Strategy where

import           Data.Vector (Vector)
import qualified Data.Vector as V
-- bubblesort, mergesort :: Vector Int -> Vector Int

unique :: Eq a => Vector a -> (Vector a -> Vector a) -> Bool
unique input sort = V.null input ||
         (let sorted = sort input
          in
            V.and $ V.zipWith (/=) sorted (V.tail sorted))

