{-# LANGUAGE ExplicitForAll #-}

module BinTree where


import           Control.Concurrent       (Chan, getChanContents, newChan,
                                           readChan, writeChan)
import           Control.Concurrent.Async (async)
import           Control.Monad            (MonadPlus, mplus, mzero)
import           Data.Foldable            (for_)
import           Data.List                (sort)
import           Data.Maybe               (catMaybes, isJust, isNothing)
import           Data.Monoid              ((<>))
import           Data.Traversable         (for, traverse)
import qualified Data.Vector              as V
import           System.Random.MWC

data Tree a
    = Nil
    | Node (Tree a) a (Tree a)
    deriving Show

{- walk a tree in ascending order, push each el to channel -}
walk :: forall a. Eq a => Tree a -> Chan (Maybe a) -> IO ()
walk tree ch = go tree >> writeChan ch Nothing
    where
        go Nil = pure ()
        go (Node l a r) = do
            go l
            writeChan ch (Just a)
            go r


{- use channels and check equivalence -}
same :: forall a. (Eq a) => Tree a -> Tree a -> IO Bool
same t1 t2 = do
    ch1 <- newChan
    ch2 <- newChan
    async $ walk t1 ch1
    async $ walk t2 ch2
    go ch1 ch2
    where
        go ch1 ch2 = do
            a <- readChan ch1
            b <- readChan ch2
            if a /= b
                then pure False
                else if isNothing a && isNothing b
                    then pure True
                    else go ch1 ch2

main :: IO ()
main = do
    -- 1
    t1 <- newTree 1
    ch <- newChan
    async $ walk t1 ch
    l <- readAll ch
    print l
    -- 1'
    t1' <- newTree 1
    ch1' <- newChan
    async $ walk t1' ch1'
    l1' <- readAll ch1'
    print l1'
    print $ l == l1'
    --
    t2 <- newTree 2
    print $ toList t2 == l

readAll :: Show a => Chan (Maybe a) -> IO [a]
readAll ch = unfoldM (readChan ch)

unfoldM :: (Monad m, MonadPlus f) => m (Maybe b) -> m (f b)
unfoldM m = whileJust m pure

whileJust :: (Monad m, MonadPlus f) => m (Maybe a) -> (a -> m b) -> m (f b)
whileJust p f = go
    where go = do
            x <- p
            case x of
                Nothing -> return mzero
                Just x  -> do
                        x  <- f x
                        xs <- go
                        pure (pure x `mplus` xs)

toList :: forall a t. (Foldable t) => t a -> [a]
toList = foldr (:) []
--provided by Foldable instance now
--walkPure :: forall a. Tree a -> [a]
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


shuffle :: forall a. (Ord a) => [Int] -> [a] -> [a]
shuffle rnds = map snd . sort . zip rnds

insert :: forall a. Ord a => a -> Tree a -> Tree a
insert a Nil = Node Nil a Nil
insert a (Node l x r)
    | a < x     = Node (insert a l) x r
    | otherwise = Node l x (insert a r)

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
  for_ (toList tree1) $ \x -> putStr (show x <> " ")
  putStrLn ""
  tree1' <- newTree 1
  tree2 <- newTree 2
  for_ (toList tree1') $ \x -> putStr (show x <> " ")
  putStrLn ""
  print $ toList tree1' == toList tree1
  print $ toList tree1 == toList tree2
