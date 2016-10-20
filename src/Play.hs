{-# LANGUAGE RecordWildCards #-}

module Play where

import qualified Data.Map.Lazy as M

-- just messing around
type Floor = Int
class Elevator a where
    moveTo :: a -> Floor -> Either String Lift
    addPassenger :: a -> Int -> Either String Lift

data Lift
    = Lift
    { current      :: Floor
    , backlog      :: [Floor]
    , people       :: Int
    , maxOccupancy :: Int }
    deriving (Show, Eq)

instance Elevator Lift where
    moveTo l@Lift{..} f = case compare f current of
        EQ -> Left "Here"
        LT -> Right $ l { current = f }
        GT -> Right $ l { current = f }

    addPassenger l@Lift{..} p = case compare (people+p) maxOccupancy of
        EQ -> Right $ l { people = people+p }
        LT -> Right $ l { people = people+p }
        GT -> Left "At max capacity"

doRun = do
    let el = Lift { current=0, backlog=[], people=2, maxOccupancy=10 }
    el <- moveTo el 2
    el <- addPassenger el 3
    el <- moveTo el 4
    addPassenger el 3


getMax :: [Integer] -> Integer
getMax = foldr max 0

getMax' :: [Integer] -> Integer
getMax' []     = 0
getMax' (x:xs) = max x (getMax xs)


data Trie a = Trie
    { value    :: Maybe a
    , children :: M.Map Char (Trie a)
    }

findInTrie :: String -> Trie a -> Maybe a
findInTrie [] a = value a
findInTrie (x:xs) a = do
    val <- M.lookup x (children a)
    findInTrie xs a
