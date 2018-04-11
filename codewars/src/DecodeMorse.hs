module DecodeMorse where

import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map, (!))


decodeMorse :: String -> String
decodeMorse = unwords . filter (not . null) . fmap (concatMap (morseCodes !)) . getWords

getWords :: String -> [[String]]
getWords = fmap words . splitOn "   "

morseCodes :: Map String String -- defined by the
morseCodes = undefined
