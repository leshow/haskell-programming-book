module Continuation where

import Control.Monad.Trans.Cont


fibCps :: Int -> Cont r Int
fibCps 0 = pure 1
fibCps 1 = pure 1
fibCps n = do
    a <- fibCps (n - 1)
    b <- fibCps (n - 2)
    pure $ a + b

runFib :: (Int -> r) -> r
runFib = runCont $ fibCps 100

fib :: Integer -> Integer
fib n = go n 1 0
  where
    go :: Integer -> Integer -> Integer -> Integer
    go 0 _ b = b
    go n a b = go (n - 1) b (a + b)
{-
let (mut a, mut b) = (0, 1);
while n > 0 {
    let temp = a + b;
    a = b;
    b = temp;
    n--;
}
return b;
-}























