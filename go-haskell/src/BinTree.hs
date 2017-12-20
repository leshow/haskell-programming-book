{-# LANGUAGE ExplicitForAll #-}

module BinTree where


import           Control.Concurrent       (Chan, getChanContents, newChan,
                                           readChan, writeChan)
import           Control.Concurrent.Async (async)
import           Control.Monad            (forever, liftM3)
import           Data.Foldable            (for_)
import           Data.List                (sort)
import           Data.Maybe               (catMaybes, isJust)
import           Data.Monoid              ((<>))
import           Data.Traversable         (for, traverse)
import qualified Data.Vector              as V
import           System.Random.MWC

data Tree a
    = Nil
    | Node (Tree a) a (Tree a)
    deriving (Show)

{- walk a tree in ascending order-}
walk :: forall a. (Eq a) => Tree a -> Chan (Maybe Int) -> IO ()
walk = undefined

-- check equivalence
same :: forall a. (Eq a) => Tree a -> Tree a -> IO Bool
same t1 t2 = undefined

main :: IO ()
main = undefined

walkPure :: forall a. Tree a -> [a]
walkPure = foldr (:) []
--walkPure Nil          = []
--walkPure (Node l a r) = walkPure l <> [a] <> walkPure r

instance Foldable Tree where
    foldMap _ Nil          = mempty
    foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Eq a => Eq (Tree a) where
    (==) Nil Nil                     = True
    (==) _ Nil                       = False
    (==) Nil _                       = False
    (==) (Node l a r) (Node l' b r') = a == b && (l == l') && (r == r')

samePure :: forall a. Eq a => Tree a -> Tree a -> Bool
samePure t1 t2 = t1 == t2


shuffle :: forall a. (Ord a) => [Int] -> [a] -> [a]
shuffle rnds xs = map snd $ sort $ zip rnds xs

insert :: forall a. a -> Tree a -> Tree a
insert = undefined

size :: Int
size = 10

newTree' :: [Int] -> Int -> Tree Int
newTree' rnds x = foldl (flip insert) Nil $ shuffle rnds $ map (x*) [1..size]

newTree :: Int -> IO (Tree Int)
newTree a = do
    rnds <- withSystemRandom . asGenIO $ \gen -> uniformVector gen size
    pure $ newTree' (V.toList rnds) a

mainPure :: IO ()
mainPure = do
  tree1 <- newTree 1
  for_ (walkPure tree1) $ \x -> putStr (show x ++ ",")
  putStrLn ""
  tree1' <- newTree 1
  tree2 <- newTree 2
  print $ samePure tree1 tree1'
  print $ samePure tree1 tree2
