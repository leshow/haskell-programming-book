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


-- mapM is just traverse -but only for lists, a more general version

-- traversable use: when you need to flip two type constructors around, or map something then flip them


data Result a b
    = Error a
    | Ok b
    deriving (Eq, Ord, Show)

instance Functor (Result a) where
    fmap _ (Error a) = Error a
    fmap f (Ok b)    = Ok (f b)

instance Applicative (Result a) where
    pure = Ok
    Ok f <*> r = fmap f r
    Error f <*> l = Error f

instance Foldable (Result a) where
    foldMap f (Error e) = mempty
    foldMap f (Ok a)    = f a

    foldr f b (Error e) = b
    foldr f b (Ok a)    = f a b

instance Traversable (Result a) where
    traverse f (Error e) = pure $ Error e
    traverse f (Ok b)    =  Ok <$> f b
