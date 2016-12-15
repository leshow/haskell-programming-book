{-# LANGUAGE RankNTypes #-}

module Ch16_nat where

-- consider if we want to transform the type and not the value, as in
-- fmap :: (a -> b) -> f a -> f b
-- we want:
-- nat :: (f -> g) -> f a -> g a
-- see here we want to transform the type, this is a 'natural transformation'
-- transforming the structure, preserving the values

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- this will not compile because we are only allowed to transform f, not a
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing  = []
-- degenerateMtl (Just a) = [a+1]

{-
    if we did,
        type Nat f g a = f a -> f g
    then both maybeToList would work as well as degenerateMtl

    provided we changed the fn types thusly, maybeToList :: Nat Maybe [] a

    we want to be precise about our problem description, and RankNTypes allows
    us to quantify over the types properly
-}
