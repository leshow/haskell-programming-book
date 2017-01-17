{-# LANGUAGE RecordWildCards #-}
-- Monads, not burritos
module Ch18 where

import           Control.Monad               (join)
import           Control.Parallel.Strategies

-- is related to fmap and applicative
-- this holds:
-- fmap f xs = xs >>= return . f
-- complete Monad instance has
-- (>>) :: m a -> m b -> m b
-- (>>=) :: m a -> (a -> m b) -> m b
-- return :: a -> m a
-- >>= is sufficient for a typeclass definition
-- >> is the sequencing operator, it discards the return value of the first result
bind
    :: Monad m
    => (a -> m b) -> m a -> m b
bind f = join . fmap f -- f ma = join $ fmap f ma

-- where join :: Monad m => m (m a) -> m a
parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' _ [] = return []
parMap' f (x:xs) = do
    x' <- rpar (f x)
    xs' <- parMap' f xs
    return (x' : xs')

runPar = runEval $ parMap' (+ 5) [1 .. 10000]

data Cow = Cow {
    name     :: String
    , age    :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c@Cow{..} = if name == "Bess" && weight > 499
                        then Nothing
                        else Just c


mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing      -> Nothing
                        Just weighty -> weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' n' a' w' = do
    name <- noEmpty n'
    age <- noNegative a'
    weight <- noNegative w'
    weightCheck $ Cow name age weight

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' n a w =
    noEmpty n >>= \name ->
        noNegative a >>= \age ->
            noNegative w >>= \weight ->
                weightCheck $ Cow name age weight

data Sum a b
    = First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second

    First a <*> _   = First a
    Second f <*> b  = fmap f b

-- m a -> (a -> m b) -> m b
-- we just unwrap the (m a) to get our a, then apply f to get m b
instance Monad (Sum a) where
    return = pure
    (>>=) (First a) _  = First a
    (>>=) (Second b) f = f b
