{-# LANGUAGE FlexibleInstances #-}

module Ch16 where

import           GHC.Arr
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

data Or a b
    = First a
    | Second b
    deriving (Eq, Show)

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

-- exercises
-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- 2

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

-- 3

data Two a b
    = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

-- 4

data Three a b c
    = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

-- 5

data Three' a b
    = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

-- 6

data Four a b c d
    = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

-- 7

data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

-- 8
-- No, because it has no type parameter that it's polymorphic over. We have nothing
-- to map f onto


-- Exercise: Possibly

data Possibly a
    = LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope     = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

-- Either

data Sum a b
    = SFirst a
    | SSecond b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (SFirst a)  = SFirst a
    fmap f (SSecond b) = SSecond (f b)

newtype Constant a b
    = Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a


-- Ch exercises

-- 1 no

-- 2
data BoolAndSomethingElse a
    = False' a | True' a

instance Functor (BoolAndSomethingElse) where
    fmap f (True' a)  = True' (f a)
    fmap f (False' a) = False' (f a)

-- 3

data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor (BoolAndMaybeSomethingElse) where
    fmap f (Truish a) = Truish (f a)
    fmap _ Falsish    = Falsish

-- 4

newtype Mu f = InF { outF :: f (Mu f) }

-- :k (* -> *) -> *
-- i dont beleive a functor instance is possible. i enabled GeneralizedNewtypeDeriving and
-- DeriveFunctor and tried to auto-derive an instance, it was NOT able to.

-- 5

data D = D (Array Word Word) Int Int

-- :k *
-- meaning no,

-- 1
data Sum'' b a =
    First' a
    | Second' b

instance Functor (Sum'' e) where
    fmap f (First' a)  = First' (f a)
    fmap f (Second' b) = Second' b

-- 2
data Company a b c
    = DeepBlue a b
    | Something c

instance Functor (Company e e') where
    fmap f (Something c)  = Something (f c)
    fmap _ (DeepBlue a b) = DeepBlue a b

-- 3

data More a b
    = L b a b
    | R a b a
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

-- 4

data Quant a b
    = Finance
    | Desk a
    | Bloor b

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 5

data K a b
    = K a

instance Functor (K a) where
    fmap _ (K a) = K a      -- can't use f here because of the way K is parametrized. can only map to b

-- 6

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) = Flip $ K' (f a)

-- 7

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)
