module Addition where

import           Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "Hello!"

main :: IO ()
main = hspec $ do
    describe "Addition " $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            (2 + 2) `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remained 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "15 multiplied by 3 is 45" $ do
            mult 15 3 `shouldBe` 45
        it "10 multiplied by 5 is 50" $ do
            mult 10 5 `shouldBe` 50

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where
        go n d count
            | n < d = (count, n)
            | otherwise = go (n-d) d (count+1)


mult :: (Eq a, Num a) => a -> a -> a
mult 0 _ = 0
mult x y = y + mult (x-1) y
