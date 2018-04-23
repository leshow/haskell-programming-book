module FlipBits where
-- | Determine the number of bits to flip a into b

import           Data.Bits
import           Data.List       (intercalate)
import           Data.List.Split (chunksOf)

flipBits :: Int -> Int -> Int
flipBits !a !b = let diff = a `xor` b in count 0 diff
  where
    count !c 0  = c
    count !c !w = count (c + 1) (w .&. (w - 1))

formatLicense :: String -> Int -> String
formatLicense s k =
    let clean = filter (/= '-') s
        split = chunksOf k (reverse clean)
    in  reverse $ intercalate "-" split

