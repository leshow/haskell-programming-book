module Ch16 where

import           Test.QuickCheck
import           Test.QuickCheck.Function
--
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- exercises
-- 1. :k *
-- 2. :k * for a, :k * -> * for T
-- 3. :k * -> * -> *

data FixMePls a
    = FixMe
    | Pls a
    deriving (Eq, Show)

instance Functor FixMePls where
    fmap _ FixMe   = FixMe
    fmap f (Pls a) = Pls (f a)

-- functor laws:
-- identity:
--      fmap id == id
-- functor composition:
--      fmap (f . g) == fmap f . fmap g
--  ex.
--      fmap ((+1) . (+1)) [1..5]
--      ==
--      fmap (+1) . fmap (+1) $ [1..5]

data WhoCares a
    = ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)

instance Functor WhoCares where
    fmap _ ItDoesnt         = ItDoesnt
    fmap f (Matter a)       = Matter (f a)
    fmap _ WhatThisIsCalled = WhatThisIsCalled

-- the identity law prevents is from changing the type constructor
-- on either side of the fmap definition, for example:
-- fmap _ ItDoesnt = WhatThisIsCalled
-- is illegal, it violates the identity law


{-
ex for
(.) :: (b -> c) -> (a -> b) -> a -> c
fmap1 :: (d -> e) -> m d -> m e
fmap1 :: (f -> g) -> n f -> n g

b = (d -> e) = (n f -> n g)
c = (m d -> m e)
a = (f -> g)

c = (m (n f) -> m (n g))

in (.) fmap fmap :: (b -> c) -> (a -> b) -> a -> c
(b -> c) and (a -> b) are already applied, so the type is of (a -> c)

so,
    (a -> c)
    ~ (f -> g) -> (m (n f) -> m (n g))
    ~ (f -> g) -> m (n f) -> m (n g)

lets try :t flip id

flip :: (a -> b -> c) -> b -> a -> c
id :: (a -> a)

a = a = b -> c

so,
    b -> a -> c          -- (first arg removed)
    ~ b -> (b -> c) -> c

-}

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

liftStuff :: IO ()
liftStuff = do
    putStr "replaceWithP' lms: "
    print (replaceWithP' lms)
    putStr "liftedReplace lms: "
    print (liftedReplace lms)
    putStr "liftedReplace' lms: "
    print (liftedReplace' lms)
    putStr "twiceLifted lms: "
    print (twiceLifted lms)
    putStr "twiceLifted' lms: "
    print (twiceLifted' lms)
    putStr "thriceLifted lms: "
    print (thriceLifted lms)
    putStr "thriceLifted' lms: "
    print (thriceLifted' lms)

-- 1
a = (+1) <$> read "[1]" :: [Int]
-- 2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- 3
c = fmap (*2) (*2)
-- 4
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
-- 5
e :: IO Integer
e = let     ioi = readIO "1" :: IO Integer
            changed = fmap (read . ("123"++) . show) ioi

    in      fmap (*3) changed


data Two a b
    = Two a b deriving (Eq, Show)

data Or a b
    = First a
    | Second b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

-- we had to partially apply the type because functors
-- fmap :: (a -> b) -> f a -> f b
-- 'f a' requires f to be of kind * -> *

-- QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g a = fmap (g . f) a == fmap g (fmap f a)

testId = quickCheck $ \x -> functorIdentity (x :: [Int])

testComp = quickCheck $ \x -> functorCompose (*2) (+2) (x :: [Int])

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' a (Fun _ f) (Fun _ g) = (fmap (g . f) a) == (fmap g . fmap f $ a)
