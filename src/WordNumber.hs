module WordNumber where

import           Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = ["zero","one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! n

digits :: Int -> [Int]
digits 0 = []
digits x
    | x < 0  = digits (-x)
    | otherwise = digits (div x 10)  ++ [mod x 10]

wordNumber :: Int -> String
wordNumber n = intercalate "-" $ map digitToWord (digits n)
-- concat $ intersperse '-' $ map digitToWord (digits n)
