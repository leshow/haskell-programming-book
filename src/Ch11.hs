module Ch11 where


{- Kinds:
    we know something is a fully applied concrete type when it has a kind signature *
    when it is * -> * or otherwise, it is like a function and waiting to be applied

    :k Bool
    Bool :: *
    --
    :k []
    [] :: * -> *

    Phantom type:
        data Phantom a = PhantomData

-}

data Doggies a
    = Husky a
    | Mastiff a
    deriving (Show, Eq)

-- :k Doggies
-- Doggies :: * -> *

-- :t Husky
-- Husky :: a -> Doggies a
{-
    1. Type constructor
    2. * -> *
    3. *
    4. Doggies Int (ghci says Num a => Doggies a  -- this is more generic)
    5. Doggies Integer
    6. Doggies [Char]
    7. both?
    8. :t a -> DogueDeBordeaux a
    9. DogueDeBordeaux [Char]
-}

data Price
    = Price Integer
    deriving (Show, Eq)

data Manufacturer
    = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline
    = PapuAir
    | CatapultsRUs
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Size
    = Sm
    | Med
    | Lg
    deriving (Eq, Show)

data Vehicle
    = Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


getManu :: Vehicle -> Manufacturer
getManu (Car man _) =  man
-- it will return a type error if called with Plane, this is poor form
-- it should return
getManuMaybe :: Vehicle -> Maybe Manufacturer
getManuMaybe (Car man _) = Just man
getManuMaybe _ = Nothing
