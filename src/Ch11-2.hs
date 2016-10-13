{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Lib where

import           Control.Applicative
import           Data.Time

data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello World"
    , DbDate (UTCTime (fromGregorian 1921 5 1)
        (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr consDate []
    where
        consDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
        consDate x y = case x of
            (DbDate t) -> t : y
            _          -> y
-- Done with filter
-- filterDbDate :: [DatabaseItem] -> [UTCTime]
-- filterDbDate db = map getDate $ filter selectDbDate db
--     where
--         getDate :: DatabaseItem -> UTCTime
--         getDate (DbDate t) = t

selectDbDate :: DatabaseItem -> Bool
selectDbDate = \case
    (DbDate t) -> True
    _          -> False

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr consNum []
    where
        consNum :: DatabaseItem -> [Integer] -> [Integer]
        consNum x y = case x of
            (DbNumber i) -> i : y
            _            -> y

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length  $ filterDbNumber db)

-- An important difference between foldr and foldl is that a left fold
-- has the successive steps of the fold as its first argument.

-- foldr max 'a' "fear is the little death"
-- foldr max (minBound :: Char) "fear is the little death"

-- foldr (\x y -> (show x)++y) "" [1..5]
-- \x y -> (show x) ++ y
-- \x y -> (++) (show x) y
-- \x -> (++) . show x
-- (++) . show

data Example = MakeExample deriving Show

{-
    Ex. For Example

    1. :t MakeExample
    > MakeExample :: Example
    2. yes, instance Show
    3.
-}

data Example' = MakeExample' Int deriving Show

-- 3. :t MakeExample'
-- > MakeExample' :: Int -> Example'
-- remember: MakeExample' is a constructor function that takes an Int and returns an Example'
-- unary constructors, like above, are the identity function for their associated type
-- they have the same cardinality

class Eq a => TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- without GeneralizedNewtypeDeriving this must be written explicitly
-- instance TooMany Goats where
--     tooMany (Goats n) = n > 43

-- :t tooMany :: TooMany a => a -> Bool
-- tooMany (Goats 10) works without specifying the type explicitly
-- like tooMany (10 :: Int) that was needed before

-- Ex Logic Goats

instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n -- use the TooMany instance for Int

instance TooMany (Int, Int) where
    tooMany (n, m) = tooMany (n + m)

-- this instance overlaps, the one above where a::Int, not sure why the book wants to do this

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (a, a') = tooMany (a + a')

{-
    Ex. Pity the Bool
    1. 4
    2.
-}

data Expr
    = Number Int
    | Add Expr Expr
    | Minus Expr Expr
    | Mult Expr Expr
    | Divide Expr Expr

arithmetic :: Expr
arithmetic = Add (Number 1) (Add (Number 2) (Number 3))

evaluate :: Expr -> Int
evaluate (Number i)       = i
evaluate (Add ex1 ex2)    = evaluate ex1 + evaluate ex2
evaluate (Minus ex1 ex2)  = evaluate ex1 - evaluate ex2
evaluate (Mult ex1 ex2)   = evaluate ex1 * evaluate ex2
evaluate (Divide ex1 ex2) = evaluate ex1 `div` evaluate ex2

-- How does your garden Grow?

data FlowerType
    = Gardenia
    | Daisy
    | Rose
    | Lilac
    deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

{- normal form:

data Garden
    = Gardenia Gardener
    | Daisy Gardener
    | Lilac Gardener
    | Rose Gardener
    deriving Show
-}
data GuessWhat =
    Chickenbutt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b
    = First a
    | Second b
    deriving (Eq, Show)

data RecordProduct a b = RecordProduct
    { pfirst  :: a
    , psecond :: b }
    deriving (Eq, Show)

mkProduct :: Product (a -> a) (b -> b)
mkProduct = Product (\a -> a) (\b -> b)

mkIdentity :: Id (a -> a)
mkIdentity = MkId $ \x -> x

-- the type Id takes an argument and the data constructor MkId takes an argument of
-- the corresponding polymorphic type.

data Twitter = Twitter deriving (Eq, Show)
data Facebook = Facebook deriving (Eq, Show)

socialNetwork :: Sum Twitter Facebook
socialNetwork = First Twitter   -- notice since it's a sum value it's either First OR Second
-- with this fn signature, we can't do something like Second Twitter, because we've defined
-- Twitter to be the First a value

mkRecord :: RecordProduct Int Int
mkRecord = RecordProduct 1 2

otherRecord :: RecordProduct Int Int
otherRecord = RecordProduct { pfirst = 1, psecond = 2 }

data OperatingSystem
    = GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show, Enum)

data ProgrammingLanguage
    = Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show, Enum)

data Programmer
    = Programmer
    { os   :: OperatingSystem
    , lang :: ProgrammingLanguage }
    deriving (Eq, Show)

-- Ex Programmers
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = a, lang = b} | a <- allOperatingSystems, b <- allLanguages]

otherallProgrammers :: [Programmer]
otherallProgrammers = liftA2 Programmer allOperatingSystems allLanguages

{-
    Ex. The Quad
    1. 4 + 4
    2. 4 * 4
    3. 4^4
    4. 2*2*2
    5. 2^2^2
    6. (4^4)^2 = 2^(4*4)

    HKT
    kinds are not types until fully applied ( * )
    * -> * is awaiting a single * before it becomes a type
    likewise, * -> * -> * must be applied twice, it's known as a higher-kinded type
    lists, are * -> * therefore are HKT

    :k (,,,)
    (,,,) :: * -> * -> * -> * -> *
-}

data Product' a b = a :&: b deriving (Show, Eq)

-- you can define infix constructors if they start with :

data BinTree a
    = Leaf
    | Node (BinTree a) a (BinTree a)
    deriving (Eq, Ord, Show)

insert' :: (Eq a, Ord a) => a -> BinTree a -> BinTree a
insert' val Leaf = Node Leaf val Leaf
insert' val (Node left a right)
    | val > a = Node left a (insert' val right)
    | val < a = Node (insert' val left) a right
    | val == a = Node left a right

map' :: (Eq a, Ord a) => (a -> b) -> BinTree a -> BinTree b
map' _ Leaf                = Leaf
map' f (Node left a right) = Node (map' f left) (f a) (map' f right)

preorder :: BinTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

foldTree :: (a -> b -> b) -> b -> BinTree a -> b
foldTree _ b Leaf                = b
foldTree f b (Node Leaf a Leaf)  = f a b
foldTree f b (Node left a right) = foldTree f (f a (foldTree f b left)) right

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f b [a]    = f a b
foldr' f b (x:xs) = f x (foldr' f b xs)


{-

Ch11 Ex
1. a
2. c
3. b
4. c

-}
