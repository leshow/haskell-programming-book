-- Monads, not burritos
module Ch18 where

import           Control.Monad               (join)
import           Control.Parallel.Strategies

-- is related to fmap and applicative
-- this holds:
-- fmap f xs = xs >>= return . f
-- complete Monad instance has
-- (>>) :: m a -> m b -> m b
-- (>>=) :: m a -> (a -> m b) -> m b
-- return :: a -> m a
-- >>= is sufficient for a typeclass definition
-- >> is the sequencing operator, it discards the return value of the first result
bind
    :: Monad m
    => (a -> m b) -> m a -> m b
bind f = join . fmap f -- f ma = join $ fmap f ma

-- where join :: Monad m => m (m a) -> m a
parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' _ [] = return []
parMap' f (x:xs) = do
    x' <- rpar (f x)
    xs' <- parMap' f xs
    return (x' : xs')

runPar = runEval $ parMap' (+ 5) [1 .. 10000]
