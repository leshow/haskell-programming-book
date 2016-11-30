module Ch14 where

import           Data.List       (sort)
import           Test.Hspec
import           Test.QuickCheck
import           WordNumber      (digitToWord, digits, wordNumber)

test1 :: IO ()
test1 = hspec $ do
    describe "digitToWord" $ do
        it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1,0,0]
    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"

testQuick :: IO ()
testQuick = hspec $ do
    describe "QC" $ do
        it "x is half of x / 2" $ do
            property $ \x -> x == halfIdentity (x :: Double)
        it "list is sorted" $ do
            property $ \x -> (listOrdered (sort x :: [Int]))
        it "addition is associative" $ do
            property $ \x y z -> plusAssociative (x :: Int) (y :: Int) (z :: Int)
        it "addition is commutative" $ do
            property $ \x y -> plusCommutative (x :: Int) (y :: Int)
        it "multiplication is associative" $ do
            property $ \x y z -> mulAssoc (x :: Int) (y :: Int) (z :: Int)
        it "mul is commutative" $ do
            property $ \x y -> mulComm (x :: Int) (y :: Int)
        it "quot rem" $ do
            property $ \x y -> (quot x y)*(y::Int) + (rem x y) == (x:: Int)
        it "div mod" $ do
            property $ \x y -> (div x y)*(y::Int) + (mod x y) == (x :: Int)

half x = x / 2

halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where
        go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

mulAssoc x y z = x * (y * z) == (x * y) * z
mulComm x y = x * y == y * x
