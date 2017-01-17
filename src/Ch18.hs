{-# LANGUAGE RecordWildCards #-}
-- Monads, not burritos
module Ch18 where

import           Control.Monad               (join, liftM, liftM2)
import           Control.Parallel.Strategies
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

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

-- right identity
-- m >>= return = m
-- left identity
-- return x >>= f = f x

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- flip (.) :: (a -> b) -> (b -> c) -> a -> c
-- example.
-- sayHi :: String -> IO String
-- readM :: Read a => String -> IO a
-- sayHi >=> readM
--
-- (String -> IO String) -> (String -> IO a) -> String -> IO a

-- Chapter Exercises

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return _ = NopeDotJpg
    (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

-- 2

data PhbtEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhbtEither b) where
    fmap f (Ch18.Left a)  = Ch18.Left $ f a
    fmap _ (Ch18.Right b) = Ch18.Right b

instance Applicative (PhbtEither b) where
    pure = Ch18.Left

    (<*>) (Ch18.Right a) _ = Ch18.Right a
    (<*>) (Ch18.Left f) b  = fmap f b

instance Monad (PhbtEither b) where
    return = pure

    (>>=) (Ch18.Right a) _ = Ch18.Right a
    (>>=) (Ch18.Left a) f  = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Ch18.Left a, Ch18.Right b]

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where (=-=) = eq

-- 3


newtype Identity a
    = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) = fmap f

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- 4
data List a
    = Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil lb         = lb
    mappend (Cons a la) lb = Cons a (la `mappend` lb)

instance Applicative List where
    pure a = Cons a Nil

    (<*>) Nil Nil        = Nil
    (<*>) Nil _          = Nil
    (<*>) _ Nil          = Nil
    (<*>) (Cons f fs) as = mappend (fmap f as) (fs <*> as)

instance Monad List where
    return = pure
    as >>= f = join $ fmap f as

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
    h <- arbitrary
    t <- genList
    frequency [(3, return $ Cons h t),
             (1, return Nil)]

instance Eq a => EqProp (List a) where (=-=) = eq


type I = Int

runMonad :: IO ()
runMonad = do
  putStr "\n-- Nope"
  quickBatch $ monad (undefined :: Nope (I, I, I))
  putStr "\n-- PEither"
  quickBatch $ monad (undefined :: PhbtEither I (I, I, I))
  putStr "\n-- Identity"
  quickBatch $ monad (undefined :: Identity (I, I, I))
  putStr "\n-- List"
  quickBatch $ monad (undefined :: List (I, I, I))

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM -- remember liftM/liftA is fmap, just with different constraints.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = do
    a' <- f a
    as' <- meh as f
    return (a':as')

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
