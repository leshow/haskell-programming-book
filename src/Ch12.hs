module Ch11 where


-- HKT are types that take types as arguments, like higher order functions.
-- Haskell Report calls HKT "type constructors"
-- Haskell Report calls fully applied types "type constants"

-- * is the kind of all standard lifted types.
-- unlifted types are of kind # and represent mostly pointers and native machine types

-- type constructors are just functions, and can be used like functions
makeJust = fmap Just [1, 2, 3]
-- [Just 1,Just 2,Just 3]

{-
    Ch Exercises
    1. :k  of a in id :: a -> a
    a :: *
    2. :k of a and f in r :: a -> f a
    a :: * , f :: * -> *
-}
