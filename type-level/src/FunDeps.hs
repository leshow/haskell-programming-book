{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module FunDeps where

data Nil
data Cons x xs

class Head list xs | list -> xs
instance Head Nil Nil
instance Head (Cons x more) x

class Append a b c | a b -> c
instance Append Nil x x
instance (Append as bs cs) => Append (Cons a as) bs (Cons a cs)

data False
data True

class AnyTrue list t | list -> t
instance AnyTrue Nil False
instance AnyTrue (Cons True xs) True
instance (AnyTrue list t) => AnyTrue (Cons False list) t
-- type family Append a b where
--     Append Nil xs = xs
--     Append (Cons x xs) ys = Append xs (Cons x ys)

class Or a b c | a b -> c
instance Or True False True
instance Or False True True
instance Or False False False
instance Or True True True

class Not a b | a -> b
instance Not False True
instance Not True False

data Z
data S a

class AddC m n r | m n -> r
instance AddC Z m n n
instance AddC m n r => AddC (S m) n (S r)