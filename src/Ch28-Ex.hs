module Ch28Ex where

import           Criterion.Main
import           Text.Show.Functions ()

-- struct DList<A> {
--      unDL : fn(&[A]) -> &[A]
-- }

newtype DList a = DL { unDL :: [a] -> [a] }

instance Show (DList a) where
    show (DL a) = show a


empty :: DList a
empty = DL id -- DL { unDL = id }
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL (const [x])
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList (DL a) = a []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
-- cons x xs = DL ((x:) . unDL xs)
-- cons x (DL xs) = DL ((x:) . xs)
cons x (DL xs) = DL (\y -> xs (x:y))
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
--snoc xs x = DL (unDL xs . (++[x]))
snoc (DL xs) x = DL (xs  . (++ [x]))
-- snoc (DL xs) x = DL (\y -> xs (y ++ [x]))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
-- append (DL x) (DL y) = DL (\z -> y (x z))
append (DL x) (DL y) = DL (x . y)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
    where go 0 xs = xs
          go n xs = go (n-1) (n : xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
    where go 0 xs = xs
          go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
    [ bench "concat list" $ whnf schlemiel 123456
    , bench "concat dlist" $ whnf constructDlist 123456
    ]
