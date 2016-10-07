module Ch12 where


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

data Nat
    = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | n >= 0 = Just $ succit n
    where
        succit :: Integer -> Nat
        succit 0 = Zero
        succit n = Succ $ succit (n-1)


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing  = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a
