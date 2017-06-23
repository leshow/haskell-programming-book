module OurExceptions where

import           Control.Exception

data NotDivThree = NotDivThree Int deriving (Eq, Show)

instance Exception NotDivThree

data NotEven = NotEven Int deriving (Eq, Show)

instance Exception NotEven

-- Exception instances here are deriveable, you don't need to write in instance

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
    | rem i 3 /= 0  = throwIO $ NotDivThree i
    | odd i         = throwIO $ NotEven i
    | otherwise     = pure i

catchNotDivThree :: IO Int -> (NotDivThree -> IO Int) -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int -> (NotDivThree -> IO Int) -> IO Int
catchNotEven = catch

data EATD
    = NotEven' Int
    | NotDivThree' Int
    deriving (Eq, Show)

instance Exception EATD

evenAndThreeDiv' :: Int -> IO Int
evenAndThreeDiv' i
    | rem i 3 /= 0      = throwIO (NotDivThree' i)
    | even i            = throwIO (NotEven' i)
    | otherwise         = pure i

{-
    type EA e = IO (Either e Int)
    try (evenAndThreeDiv' 0) :: EA EATD
    > Left (NotEven' 0)
    try (evenAndThreeDiv' 3) :: EA EATD
    > Right 3
-}
