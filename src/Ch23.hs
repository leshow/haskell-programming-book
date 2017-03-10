module Ch23 where

-- state is appropriate when you want to express your program in term so of
-- values that can potentially vary each evaluation step

import           Control.Exception (catch)
import           System.Random

data Die =
    DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show, Enum)

intToDie :: Int -> Die
intToDie n  | n > 0 && n <= 6   = [DieOne .. DieSix]!!(n-1) -- i didn't feel like typing all those cases, I admit the perf is shit here probably
            | otherwise         = error "got a non 1-6 integer"

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 0
        (d1,s1) = randomR (1,6) s
        (d2,s2) = randomR (1,6) s1
        (d3,_) = randomR (1,6) s2
    (intToDie d1, intToDie d2, intToDie d3)

-- those code is deterministic, produces same results
