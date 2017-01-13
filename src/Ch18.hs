-- Monads, not burritos
module Ch18 where


-- is related to fmap and applicative
-- this holds:
-- fmap f xs = xs >>= return . f

-- complete Monad instance has
-- (>>) :: m a -> m b -> m b
-- (>>=) :: m a -> (a -> m b) -> m b
-- return :: a -> m a

-- >>= is sufficient for a typeclass definition
-- >> is the sequencing operator, it discards the return value of the first result
