module Ch21 where

-- Traversable is a superclass of Foldable that defines a type which
-- maps each element of a structure to an action, and evaluates actions from left
-- to right.

-- {-# MINIMAL traverse | sequenceA #-}
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse f = sequenceA . fmap f

-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA = traverse id
-- you can see from the fn signature it flips the structure of it's argument around t <=> f
-- fmap sum $ sequenceA [Just 1, Just 2, Just 3]
-- Just 6
-- sequenceA flipped: [Maybe Int] <=> Maybe [Int]
-- however if there's a Nothing in the list, the whole thing will return Nothing
-- fmap sum $ sequenceA [Just 1, Just 2, Nothing]
-- Nothing
