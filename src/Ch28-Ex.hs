module Main where

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
singleton = DL . (:)
--singleton x = DL (\z -> x:z)
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


data Error
    = Err1 String
    | Err2 String
    deriving (Show, Eq)

data Thing = Thing
    { field1 :: Int
    , field2 :: String
    } deriving (Eq, Show)

doEither :: Int -> String -> Either Error Thing
doEither x y = do
    f1 <- validate1 x
    f2 <- validate2 y
    return $ Thing f1 f2

dofmapEither :: Int -> String -> Either Error Thing
dofmapEither x y = Thing <$> validate1 x <*> validate2 y

doManual :: Int -> String -> Either Error Thing
doManual x y =
    case validate1 x of
        Left err -> Left err
        Right x' -> case validate2 y of
                        Left err' -> Left err'
                        Right y'  -> Right $ Thing x' y'

validate1 :: Int -> Either Error Int
validate1 = undefined

validate2 :: String -> Either Error String
validate2 = undefined


