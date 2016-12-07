module Ch16 where

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
