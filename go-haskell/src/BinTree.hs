{-# LANGUAGE ExplicitForAll #-}

module BinTree where


import           Control.Concurrent       (Chan, getChanContents, newChan,
                                           readChan, writeChan)
import           Control.Concurrent.Async (async)
import           Control.Monad            (forever)
import           Data.Foldable            (for_)
import           Data.Maybe               (catMaybes, isJust)
import           Data.Traversable         (for, traverse)


data Tree a
    = Nil
    | Node (Tree a) a (Tree a)
    deriving (Show)

{- walk a tree in ascending order-}
walk :: forall a. Eq a => Tree a -> Chan (Maybe Int) -> IO ()
walk = undefined

-- check equivalence
same :: forall a. Eq a => Tree a -> Tree a -> IO Bool
same t1 t2 = undefined

main :: IO ()
main = undefined
