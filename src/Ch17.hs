module Ch17 where

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- class Functor f => Applicative f where
--      pure :: a -> f a
--      (<*>) :: f (a -> b) -> f a -> f b
-- every applicative must also have a functor instance
-- Control.Applicative also provides liftA liftA2 liftA3
-- liftA is fmap but with a applicative constraint instead of functor
-- liftA2 and A3 are fmap but can take more arguments
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- Applicatives are Monoidal Functors

-- also, you can define functor in terms of fmap, for example
-- fmap f x == pure f <*> x
-- and
-- fmap (+1) [1,2,3] == pure (+1) <*> [1,2,3]


-- ("Woo", (+1)) <*> ("Hoo!", 0) -- ("WooHoo!", 1)

-- notice the strings concatenated. this is due to Monoid instances for that value
