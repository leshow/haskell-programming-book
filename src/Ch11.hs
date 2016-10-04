module Ch11 where

import           Data.Char
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
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


getManu :: Vehicle -> Manufacturer
getManu (Car man _) =  man
-- it will return a type error if called with Plane, this is poor form
-- it should return
getManuMaybe :: Vehicle -> Maybe Manufacturer
getManuMaybe (Car man _) = Just man
getManuMaybe _           = Nothing



data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf               = b
foldTree f b (Node Leaf a Leaf) = f a b
foldTree f b (Node l a r)       = foldTree f (f a (foldTree f b l)) r

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _          = True
isSubsequenceOf (x:xs) search = elem x search && isSubsequenceOf xs search

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map capWord $ words sentence
    where
        capWord word@(x:xs) = (word, toUpper x : xs)

type Digit = Char
-- validButtons = "1234567890*#"
type Presses = Int

data Button = Button Char String
    deriving (Show)

data DaPhone = DaPhone [Button]

daphone :: DaPhone
daphone =
    DaPhone [
    Button '1' ""
    ,Button '2' "ABC"
    ,Button '3' "DEF"
    ,Button '4' "GHI"
    ,Button '5' "JKL"
    ,Button '6' "MNO"
    ,Button '7' "PQRS"
    ,Button '8' "TUV"
    ,Button '9' "WXYZ"
    ,Button '*' "^"
    ,Button '0' "+_"
    ,Button '#' ".,"
    ]

convo :: [String]
convo =
    [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"
    ]

-- 'a' -> [('2',1)]
-- 'A' -> [('*', 1),('2',1)]

-- reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
-- reverseTaps (DaPhone (Button b digits)) char =
--     where
--         definitions
