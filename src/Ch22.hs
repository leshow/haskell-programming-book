{-# LANGUAGE InstanceSigs #-}

module Ch22 where


import           Control.Applicative
import           Data.Char
import           Data.Monoid         ((<>))

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop -- fmap boop doop x == (*2) ((+10) x)

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop
-- in these 2 scenarios, the argument will get passed to both boop and doop in parallel and the results added together
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- fn composition is equivalent to application in this scenario
-- (+) . (*2) == (+) <$> (*2)

{-
    instance Functor ((->) r) where
        fmap = (.)

    (->) takes 2 arguments and therefore has kind * -> * -> *
    so we know we need to apply an argument to get a Functor.
    With the type:
        data (->) r b

    you lift over the (->) r and only transform the b value. (Like we would do with Either a b)
    `a` is conventionally called r for Reader.

    ((->) r) is the same as (r ->) and is r -> b when fully applied
-- 1
    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    fmap :: (b -> c) -> f b -> f c
-- 2
    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    fmap :: (b -> c) -> ((->) a) b -> ((->) a)
-- 2
    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    fmap :: (b -> c) -> (a -> b) -> (a -> c)
-}
cap :: String -> String
cap = fmap toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = liftA2 (,) cap rev

tupled' :: String -> (String, String)
tupled' = do
    c <- cap
    r <- rev
    return (c, r)

tupled'' :: String -> (String, String)
tupled'' = (,) <$> cap <*> rev

tupled''' :: String -> (String, String)
tupled''' = rev <$> cap >>= (,)


newtype Reader r a = Reader { runReader :: r -> a } -- r is the type we're 'reading' and a is the result type

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ \r -> f (ra r)    -- or Reader $ (f . ra)

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog = Dog {
    dogsName      :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers = Person   (HumanName "big bird")
                (DogName "barkley")
                (Address "sesame street")
ev :: Person
ev = Person (HumanName "Evan")
            (DogName "papu")
            (Address "Ottawa")

-- without reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address -- liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fc = f <$> fa <*> fc

asks :: (r -> a) -> Reader r a
asks f = Reader f   -- asks = Reader

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ \w -> a
    -- pure a = Reader $ \w -> a
    -- pure a = Reader $ const a
    -- pure = Reader . const
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    Reader ra >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

getDogRM' :: Reader Person Dog
getDogRM' = Dog <$> Reader dogName <*> Reader address

getD :: Person -> Dog
getD = runReader getDogRM'

getDogRM'' :: Reader Person Dog
getDogRM'' = do
    name <- Reader dogName
    address <- Reader address
    return $ Dog name address
