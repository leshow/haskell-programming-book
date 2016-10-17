{-# LANGUAGE RecordWildCards #-}

module Play where
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
