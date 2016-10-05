{-# LANGUAGE GADTs #-}

module Ch11 where

import           Data.Char
import           Data.List
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

isSubsequenceOff :: Eq a => [a] -> [a] -> Bool
isSubsequenceOff [] _          = True
isSubsequenceOff (x:xs) search = elem x search && isSubsequenceOff xs search

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
    deriving (Show)

daphone :: DaPhone
daphone =
    DaPhone
    [Button '1' "1", Button '2' "abc2", Button '3' "def3"
    , Button '4' "ghi4", Button '5' "jkl5", Button '6' "mno6"
    , Button '7' "pqrs7", Button '8' "tub8", Button '9' "wxyz9"
    , Button '*' "^", Button '0' "+ 0", Button '#' ".,"]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"
    ]

idx :: Char -> String -> Int
idx c xs = length $ takeWhile (/= c) xs

-- 'a' -> [('2',1)]
-- 'A' -> [('*', 1),('2',1)]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) _ = []
reverseTaps (DaPhone ((Button b ls) : xs)) char
    | char `elem` ls            = [(b, 1 + idx char ls)]
    | toLower char `elem` ls    = [('*',1), (b, 1 + idx (toLower char) ls)]
    | otherwise                 = reverseTaps (DaPhone xs) char

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead _ []         = []
cellPhonesDead phone (x:xs) = reverseTaps phone x ++ cellPhonesDead phone xs

-- this is similar to elemIndex from Data.List
phoneIndex :: Char -> String -> [Int]
phoneIndex char xs = [i | (x,i) <- zip xs [1..], (==) char x]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, b) rest -> b + rest) 0

tapsForChar :: DaPhone -> Char -> Presses
tapsForChar phone = fingerTaps . reverseTaps phone

countChar :: String -> [(Int, Char)]
countChar sentence = zipWith (\a b -> (length a, b)) (group sentence) sentence

-- book says most popular letter but describes finding the most costly letter to type
-- i solved most costly letter as it's a more interesting problem
mostPopularLetter :: String -> Char
mostPopularLetter sentence = snd . maximum $ map (\(i, c) -> (i*tapsForChar daphone c, c)) (countChar sentence)

-- again, this is the most costly letter to type out of the whole convo
coolestLtr :: [String] -> Char
coolestLtr sentList = mostPopularLetter $ concat sentList

-- ill find the word with the most occurences
coolestWord :: [String] -> String
coolestWord sentList = head . snd . maximum . map (\a -> (length a, a)) . group . words $ concatMap (\a -> " " ++ a) sentList


longestWord = snd . maximum . sort . map (\a -> (length a, a)) . words $ concatMap (\a -> " " ++ a) convo
-- I've done the below so many times I can do it blindfolded, what's more interesting to me is
-- extending DSLs with GADTs for more expressiveness
data Expr
    = I Integer
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr

eval :: Expr -> Integer
eval (I i)         = i
eval (Add ex1 ex2) = eval ex1 + eval ex2
eval (Sub ex1 ex2) = eval ex1 - eval ex2
eval (Mul ex1 ex2) = eval ex1 * eval ex2

printExpr :: Expr -> String
printExpr (I i)         = show i
printExpr (Add ex1 ex2) = printExpr ex1 ++ " + " ++ printExpr ex2
printExpr (Sub ex1 ex2) = printExpr ex1 ++ " - " ++ printExpr ex2
printExpr (Mul ex1 ex2) = printExpr ex1 ++ " * " ++ printExpr ex2

-- a datatype is how we declare and create data for our functions to receive as inputs
-- datatype is made of a type constructor[1] and zero or more data constructors[2] which have
-- zero or more arguments[3]
-- data TypeConstructor = DataConstructor Int
--          [1]             [2]             [3]

-- just messing around
data Exp a where
    I' :: Int -> Exp Int
    B :: Bool -> Exp Bool
    Add' :: Exp Int -> Exp Int -> Exp Int
    Sub' :: Exp Int -> Exp Int -> Exp Int
    Eq :: Exp Bool -> Exp Bool -> Exp Bool
